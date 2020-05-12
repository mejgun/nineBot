module Main
  ( main
  )
where

import           Lib                            ( getPost )
-- import           Types                          ( caption )
import           Logger                         ( Logger
                                                , startLogger
                                                )

import           API.AuthorizationState
import           API.Functions.CheckAuthenticationBotToken
import           API.Functions.CheckDatabaseEncryptionKey
--import           API.Functions.SendMessageAlbum
import           API.ConnectionState
import           API.Functions.SetLogVerbosityLevel
import           API.Functions.SetTdlibParameters
import           API.GeneralResult
import qualified API.Message                   as M
import qualified API.Update                    as U
import           Data.Maybe                     ( fromMaybe )
import           Defaults
import           TDLib




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
  --a <- getGag "m.9gag.com/gag/agAy16W"
  a <- getPost "vk.com/wall-52537634_1543772"
  print a
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
handleResult (Update (U.UpdateNewMessage { U.message = Just msg })) _ st l = do
  l $ show msg
  return st { incomingMessages = msg : incomingMessages st }
handleResult _ _ st _ = return st


emptyState :: State
emptyState = State { currentExtra     = Nothing
                   , answeringMessage = Nothing
                   , failedMessages   = []
                   , incomingMessages = []
                   , online           = False
                   }
