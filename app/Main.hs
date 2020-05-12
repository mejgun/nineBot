{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           API.AuthorizationState
import           API.ConnectionState
import qualified API.FormattedText             as FT
import           API.Functions.CheckAuthenticationBotToken
import           API.Functions.CheckDatabaseEncryptionKey
import qualified API.Functions.ResendMessages  as RM
import qualified API.Functions.SendMessage     as SM
import qualified API.Functions.SendMessageAlbum
                                               as SMA
import           API.Functions.SetLogVerbosityLevel
import           API.Functions.SetTdlibParameters
import           API.GeneralResult
import qualified API.InputFile                 as IF
import qualified API.InputMessageContent       as IMC
import qualified API.Message                   as M
import           API.MessageContent
import qualified API.TextEntity                as TE
import qualified API.TextEntityType            as TET
import qualified API.Update                    as U
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                , isNothing
                                                )
import           Defaults
import           Lib                            ( getPost )
import           Logger                         ( Logger
                                                , startLogger
                                                )
import           TDLib
import           Types




data State = State
    { currentExtra     :: Maybe String
    , answeringMessage :: Maybe M.Message
    , failedMessages   :: [M.Message]
    , incomingMessages :: [M.Message]
    , online           :: Bool
    }
    deriving (Show)

main :: IO ()
main = do
  logToFile <- startLogger
  client    <- create
  send client SetLogVerbosityLevel { new_verbosity_level = Just 2 }
  mainLoop client emptyState logToFile

mainLoop :: Client -> State -> Logger -> IO ()
mainLoop c st l = do
  r <- receive c
  case r of
    Nothing                          -> mainLoop c st l
    Just (ResultWithExtra res extra) -> do
      print st
      print res
      newSt <- case extra of
        Just xt -> handleExtra xt res st l
        Nothing -> handleResult res c st l
      mainLoop c newSt l

handleAuthState :: Client -> AuthorizationState -> IO ()
handleAuthState c AuthorizationStateWaitTdlibParameters =
  send c SetTdlibParameters { parameters = Just defaultTdlibParameters }
handleAuthState c (AuthorizationStateWaitEncryptionKey _) =
  send c CheckDatabaseEncryptionKey { encryption_key = Just "randomencryption" }
handleAuthState c AuthorizationStateWaitPhoneNumber = do
  t <- putStrLn "Enter bot token" >> getLine
  send c CheckAuthenticationBotToken { token = Just t }
handleAuthState _ _ = return ()

handleConnState :: Client -> ConnectionState -> State -> IO State
handleConnState _ ConnectionStateReady st = return $ st { online = True }
handleConnState _ _                    st = return $ st { online = False }

handleExtra :: String -> GeneralResult -> State -> Logger -> IO State
handleExtra extra res st l
  | extra == fromMaybe "" (currentExtra st) = do
    case res of
      Error e -> l $ show e
      _       -> return ()
    return st { currentExtra = Nothing }
  | otherwise = return st

handleResult :: GeneralResult -> Client -> State -> Logger -> IO State
handleResult (Update (U.UpdateAuthorizationState { U.authorization_state = Just s })) c st _
  = handleAuthState c s >> return st
handleResult (Update (U.UpdateConnectionState { U.state = Just conSt })) c st _
  = handleConnState c conSt st
handleResult (Update (U.UpdateNewMessage { U.message = Just msg })) _ st l
  | M.chat_id msg == M.sender_user_id msg = l (show msg)
  >> return st { incomingMessages = msg : incomingMessages st }
  | otherwise = return st
handleResult (Update e@(U.UpdateMessageSendFailed { U.message = Just msg })) _ st l
  = l (show e) >> return st { failedMessages = msg : failedMessages st }
handleResult _ c st l
  | isNothing (currentExtra st) && not (null (failedMessages st)) && online st
  = do
    let msg = head $ failedMessages st
    extra <- sendWExtra
      c
      RM.ResendMessages { RM.chat_id     = M.chat_id msg
                        , RM.message_ids = Just [fromMaybe 0 (M._id msg)]
                        }
    return st { failedMessages = tail (failedMessages st)
              , currentExtra   = Just extra
              }
  | isNothing (currentExtra st) && not (null (incomingMessages st)) && online st
  = do
    let msg = head $ incomingMessages st
    newSt <-
      if (M.chat_id msg == M.sender_user_id msg) && isJust (M.chat_id msg)
        then do
          let lnk = fromMaybe "fail" (getMessageText msg)
          post <- getPost lnk
          sendReply c st post msg l
        else return st
    return newSt { incomingMessages = tail (incomingMessages st) }
  | otherwise
  = return st

getMessageText :: M.Message -> Maybe String
getMessageText m = do
  case M.content m of
    Just MessageText { _text = t } -> case t of
      Just FT.FormattedText { FT.text = Just txt } -> Just txt
      _ -> Nothing
    _ -> Nothing

emptyState :: State
emptyState = State { currentExtra     = Nothing
                   , answeringMessage = Nothing
                   , failedMessages   = []
                   , incomingMessages = []
                   , online           = False
                   }

sendReply :: Client -> State -> Types.Result -> M.Message -> Logger -> IO State
sendReply c st res msg l = do
  extra <- case res of
    Left UnknownSite -> sendWExtra c $ sendTextMsg chat replyTo "unknown site"
    Left (NetErr e)  -> do
      l $ show e
      sendWExtra c $ sendTextMsg chat replyTo "some error. try again later"
    Left (UrlErr e) -> do
      l $ show e
      sendWExtra c $ sendTextMsg chat replyTo "broken link"
    Right resp -> if null (Types.photo resp) && null (Types.video resp)
      then sendWExtra c $ sendTextMsg chat replyTo "no media found"
      else sendWExtra c $ sendAlbumMsgs chat replyTo resp
  return $ st { currentExtra = Just extra }
 where
  chat    = M.chat_id msg
  replyTo = M._id msg

sendAlbumMsgs :: Maybe Int -> Maybe Int -> Types.Resp -> SMA.SendMessageAlbum
sendAlbumMsgs cID rID resp = do
  SMA.SendMessageAlbum { SMA.chat_id                = cID
                       , SMA.reply_to_message_id    = rID
                       , SMA.options                = Nothing
                       , SMA.input_message_contents = Just reduceAddLink
                       }
 where
  reduceAddLink :: [IMC.InputMessageContent]
  reduceAddLink =
    let c = take 10 content
    in  if null c
          then c
          else
            let (h : t) = c
                newH    = addLinkNCaption h
            in  newH : t
  addLinkNCaption :: IMC.InputMessageContent -> IMC.InputMessageContent
  addLinkNCaption imc = imc
    { IMC.caption = Just FT.FormattedText
      { FT.text     = Just ("link " ++ take 200 (Types.caption resp))
      , FT.entities = Just
        [ TE.TextEntity
            { TE._type   = Just
              (TET.TextEntityTypeTextUrl { TET.url = Just (Types.url resp) })
            , TE._length = Just 4
            , TE.offset  = Just 0
            }
        ]
      }
    }
  content :: [IMC.InputMessageContent]
  content = (map v (Types.video resp)) ++ (map p (Types.photo resp))
  v :: String -> IMC.InputMessageContent
  v s = IMC.InputMessageVideo
    { IMC.ttl                    = Nothing
    , IMC.caption                = Nothing
    , IMC.supports_streaming     = Nothing
    , IMC.height                 = Nothing
    , IMC.width                  = Nothing
    , IMC.duration               = Nothing
    , IMC.added_sticker_file_ids = Nothing
    , IMC.thumbnail              = Nothing
    , IMC.video                  = Just IF.InputFileRemote { IF._id = Just s }
    }
  p :: String -> IMC.InputMessageContent
  p s = IMC.InputMessagePhoto
    { IMC.ttl                    = Nothing
    , IMC.caption                = Nothing
    , IMC.height                 = Nothing
    , IMC.width                  = Nothing
    , IMC.added_sticker_file_ids = Nothing
    , IMC.thumbnail              = Nothing
    , IMC.photo                  = Just IF.InputFileRemote { IF._id = Just s }
    }

sendTextMsg :: Maybe Int -> Maybe Int -> String -> SM.SendMessage
sendTextMsg cID rID msg = SM.SendMessage
  { SM.chat_id               = cID
  , SM.reply_to_message_id   = rID
  , SM.reply_markup          = Nothing
  , SM.options               = Nothing
  , SM.input_message_content = Just IMC.InputMessageText
                                 { IMC.clear_draft = Nothing
                                 , IMC.disable_web_page_preview = Nothing
                                 , IMC.text = Just FT.FormattedText
                                                { FT.text     = Just msg
                                                , FT.entities = Nothing
                                                }
                                 }
  }
