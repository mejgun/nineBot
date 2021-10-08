module Lib.Bot.Impl.Telegram
  ( newHandle,
  )
where

import API.AuthorizationState
import API.ConnectionState
import qualified API.FormattedText as FT
import API.Functions.CheckAuthenticationBotToken
import API.Functions.CheckDatabaseEncryptionKey
import qualified API.Functions.ResendMessages as RM
import qualified API.Functions.SendMessage as SM
import qualified API.Functions.SendMessageAlbum as SMA
import API.Functions.SetLogVerbosityLevel
import API.Functions.SetTdlibParameters
import API.GeneralResult
import qualified API.InputFile as IF
import qualified API.InputMessageContent as IMC
import qualified API.Message as M
import API.MessageContent
import qualified API.MessageSender as MS
import qualified API.Ok
import qualified API.TextEntity as TE
import qualified API.TextEntityType as TET
import qualified API.Update as U
import Data.Maybe
  ( fromJust,
    fromMaybe,
    isJust,
    isNothing,
  )
import Defaults
import qualified Lib.Bot as Bot
import qualified Lib.Inet as Inet
import qualified Lib.Logger as Logger
import qualified Lib.PostContent as PostContent
import TDLib

newHandle :: String -> String -> IO Bot.Handle
newHandle tgToken encryptionKey =
  return $ Bot.Handle {Bot.start = startBot tgToken encryptionKey}

data BotState = BotState
  { currentExtra :: Maybe String,
    answeringMessage :: Maybe M.Message,
    failedMessages :: [M.Message],
    incomingMessages :: [M.Message],
    online :: Bool,
    client :: Client,
    inetH :: Inet.Handle,
    logH :: Logger.Handle,
    tdlibToken :: String,
    tdlibEncryptionKey :: String
  }

startBot :: String -> String -> Logger.Handle -> Inet.Handle -> IO ()
startBot tgToken encryptionKey logHandler inetHandler = do
  cl <- create
  send cl SetLogVerbosityLevel {new_verbosity_level = Just 2}
  mainLoop $ emptyBotState cl inetHandler logHandler tgToken encryptionKey

mainLoop :: BotState -> IO ()
mainLoop botState = do
  r <- receive $ client botState
  case r of
    -- Nothing                          -> mainLoop botState
    Nothing ->
      if not $ null $ incomingMessages botState
        then handleResult (Ok API.Ok.Ok) botState >>= mainLoop
        else mainLoop botState
    Just (ResultWithExtra res extra) -> do
      print res
      newSt <- case extra of
        Just xt -> handleExtra xt res botState
        Nothing -> handleResult res botState
      mainLoop newSt

handleAuthState :: BotState -> AuthorizationState -> IO ()
handleAuthState botState AuthorizationStateWaitTdlibParameters =
  send
    (client botState)
    SetTdlibParameters {parameters = Just defaultTdlibParameters}
handleAuthState botState (AuthorizationStateWaitEncryptionKey _) =
  send
    (client botState)
    CheckDatabaseEncryptionKey
      { encryption_key = Just (tdlibEncryptionKey botState)
      }
handleAuthState botState AuthorizationStateWaitPhoneNumber =
  send
    (client botState)
    CheckAuthenticationBotToken {token = Just (tdlibToken botState)}
handleAuthState _ _ = return ()

handleConnState :: ConnectionState -> BotState -> IO BotState
handleConnState ConnectionStateReady botState =
  return $ botState {online = True}
handleConnState _ botState = return $ botState {online = False}

handleExtra :: String -> GeneralResult -> BotState -> IO BotState
handleExtra extra res botState
  | extra == fromMaybe "" (currentExtra botState) = do
    case res of
      Error e -> Logger.logg (logH botState) $ show e
      _ -> return ()
    return botState {currentExtra = Nothing}
  | otherwise = return botState

handleResult :: GeneralResult -> BotState -> IO BotState
handleResult (Update U.UpdateAuthorizationState {U.authorization_state = Just s}) botState =
  handleAuthState botState s >> return botState
handleResult (Update U.UpdateConnectionState {U.state = Just conSt}) botState =
  handleConnState conSt botState
handleResult (Update U.UpdateNewMessage {U.message = Just msg}) botState
  | isJust (M.sender msg)
      && isJust (M.chat_id msg)
      && M.chat_id msg
      == MS.user_id (fromJust (M.sender msg)) =
    Logger.logg (logH botState) (show msg)
      >> return botState {incomingMessages = msg : incomingMessages botState}
  | otherwise =
    return botState
handleResult (Update e@U.UpdateMessageSendFailed {U.message = Just msg}) botState =
  Logger.logg (logH botState) (show e)
    >> return botState {failedMessages = msg : failedMessages botState}
handleResult _ botState
  | isNothing (currentExtra botState)
      && not (null (failedMessages botState))
      && online botState =
    resendFirstFailedMessage botState
  | isNothing (currentExtra botState)
      && not (null (incomingMessages botState))
      && online botState =
    answerToFirstReceivedMessage botState
  | otherwise =
    return botState

getMessageText :: M.Message -> Maybe String
getMessageText m = case M.content m of
  Just MessageText {_text = t} -> case t of
    Just FT.FormattedText {FT.text = Just txt} -> Just txt
    _ -> Nothing
  _ -> Nothing

emptyBotState ::
  Client -> Inet.Handle -> Logger.Handle -> String -> String -> BotState
emptyBotState tdlibClient inetHandler logHandler tgToken encryptionKey =
  BotState
    { currentExtra = Nothing,
      answeringMessage = Nothing,
      failedMessages = [],
      incomingMessages = [],
      online = False,
      client = tdlibClient,
      inetH = inetHandler,
      logH = logHandler,
      tdlibToken = tgToken,
      tdlibEncryptionKey = encryptionKey
    }

resendFirstFailedMessage :: BotState -> IO BotState
resendFirstFailedMessage botState = do
  let msg = head $ failedMessages botState
  extra <-
    sendWExtra
      (client botState)
      RM.ResendMessages
        { RM.chat_id = M.chat_id msg,
          RM.message_ids = Just [fromMaybe 0 (M._id msg)]
        }
  return
    botState
      { failedMessages = tail (failedMessages botState),
        currentExtra = Just extra
      }

answerToFirstReceivedMessage :: BotState -> IO BotState
answerToFirstReceivedMessage botState = do
  let msg = head $ incomingMessages botState
  newSt <-
    if isJust (M.sender msg)
      && isJust (M.chat_id msg)
      && M.chat_id msg
      == MS.user_id (fromJust (M.sender msg))
      then do
        let lnk = fromMaybe "fail" (getMessageText msg)
        post <- PostContent.getPost (inetH botState) lnk
        sendReply botState post msg
      else return botState
  return newSt {incomingMessages = tail (incomingMessages botState)}

sendReply :: BotState -> PostContent.Result -> M.Message -> IO BotState
sendReply botState res msg = do
  extra <- case res of
    Left PostContent.UnknownSite ->
      sendWExtra (client botState) $ sendTextMsg chat replyTo "unknown site"
    Left (PostContent.NetErr e) -> do
      Logger.logg (logH botState) $ show e
      sendWExtra (client botState) $
        sendTextMsg chat replyTo "some error. try again later"
    Left (PostContent.UrlErr e) -> do
      Logger.logg (logH botState) $ show e
      sendWExtra (client botState) $ sendTextMsg chat replyTo "broken link"
    Right resp ->
      if null (PostContent.photo resp) && null (PostContent.video resp)
        then
          sendWExtra (client botState) $
            sendTextMsg chat replyTo "no media found"
        else sendWExtra (client botState) $ sendAlbumMsgs chat replyTo resp
  return $ botState {currentExtra = Just extra}
  where
    chat = M.chat_id msg
    replyTo = M._id msg

sendAlbumMsgs ::
  Maybe Int -> Maybe Int -> PostContent.Resp -> SMA.SendMessageAlbum
sendAlbumMsgs cID rID resp =
  SMA.SendMessageAlbum
    { SMA.chat_id = cID,
      SMA.reply_to_message_id = rID,
      SMA.options = Nothing,
      SMA.input_message_contents = Just reduceAddLink,
      SMA.message_thread_id = Nothing
    }
  where
    reduceAddLink :: [IMC.InputMessageContent]
    reduceAddLink =
      let c = take 10 content
       in if null c
            then c
            else
              let (h : t) = c
                  newH = addLinkNCaption h
               in newH : t
    addLinkNCaption :: IMC.InputMessageContent -> IMC.InputMessageContent
    addLinkNCaption imc =
      imc
        { IMC.caption =
            Just
              FT.FormattedText
                { FT.text = Just ("link " ++ take 200 (PostContent.caption resp)),
                  FT.entities =
                    Just
                      [ TE.TextEntity
                          { TE._type =
                              Just
                                ( TET.TextEntityTypeTextUrl
                                    { TET.url = Just (PostContent.url resp)
                                    }
                                ),
                            TE._length = Just 4,
                            TE.offset = Just 0
                          }
                      ]
                }
        }
    content :: [IMC.InputMessageContent]
    content =
      (map v (PostContent.video resp)) ++ (map pp (PostContent.photo resp))
    v :: String -> IMC.InputMessageContent
    v s =
      IMC.InputMessageVideo
        { IMC.ttl = Nothing,
          IMC.caption = Nothing,
          IMC.supports_streaming = Nothing,
          IMC.height = Nothing,
          IMC.width = Nothing,
          IMC.duration = Nothing,
          IMC.added_sticker_file_ids = Nothing,
          IMC.thumbnail = Nothing,
          IMC.video = Just IF.InputFileRemote {IF._id = Just s}
        }
    pp :: String -> IMC.InputMessageContent
    pp s =
      IMC.InputMessagePhoto
        { IMC.ttl = Nothing,
          IMC.caption = Nothing,
          IMC.height = Nothing,
          IMC.width = Nothing,
          IMC.added_sticker_file_ids = Nothing,
          IMC.thumbnail = Nothing,
          IMC.photo = Just IF.InputFileRemote {IF._id = Just s}
        }

sendTextMsg :: Maybe Int -> Maybe Int -> String -> SM.SendMessage
sendTextMsg cID rID msg =
  SM.SendMessage
    { SM.chat_id = cID,
      SM.reply_to_message_id = rID,
      SM.reply_markup = Nothing,
      SM.options = Nothing,
      SM.message_thread_id = Nothing,
      SM.input_message_content =
        Just
          IMC.InputMessageText
            { IMC.clear_draft = Nothing,
              IMC.disable_web_page_preview = Nothing,
              IMC.text =
                Just
                  FT.FormattedText
                    { FT.text = Just msg,
                      FT.entities = Nothing
                    }
            }
    }
