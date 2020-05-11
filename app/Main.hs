module Main
  ( main
  )
where

import           Lib                            ( getPost )
-- import           Types                          ( caption )

import           API.AuthorizationState
import           API.Functions.CheckAuthenticationBotToken
import           API.Functions.CheckDatabaseEncryptionKey
--import           API.Functions.SendMessageAlbum
import           API.Functions.SetLogVerbosityLevel
import           API.Functions.SetTdlibParameters
import           API.GeneralResult
import qualified API.Message                   as M
import           Defaults
import           TDLib
import           Data.Maybe                     ( fromMaybe )
import qualified API.Update                    as U
import           API.ConnectionState




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
  client <- create
  send client SetLogVerbosityLevel { new_verbosity_level = Just 2 }
  mainLoop client emptyState

mainLoop :: Client -> State -> IO ()
mainLoop c st = do
  r <- receive c
  case r of
    Nothing                          -> mainLoop c st
    Just (ResultWithExtra res extra) -> do
      print res
      newSt <- case extra of
        Just xt -> handleExtra xt res st
        Nothing -> handleResult res c st
      mainLoop c newSt

handleAuthState :: Client -> AuthorizationState -> IO ()
handleAuthState c AuthorizationStateWaitTdlibParameters =
  send c SetTdlibParameters { parameters = Just defaultTdlibParameters }
handleAuthState c (AuthorizationStateWaitEncryptionKey _) =
  send c CheckDatabaseEncryptionKey { encryption_key = Just "randomencryption" }
handleAuthState c AuthorizationStateWaitPhoneNumber = do
  putStrLn "Enter bot token"
  t <- getLine
  send c CheckAuthenticationBotToken { token = Just t }
handleAuthState _ _ = return ()

handleConnState :: Client -> ConnectionState -> State -> IO State
handleConnState _ ConnectionStateReady st = return $ st { online = True }
handleConnState _ _                    st = return $ st { online = False }

handleExtra :: String -> GeneralResult -> State -> IO State
handleExtra extra res st
  | extra == fromMaybe "" (currentExtra st) = do
    case res of
      Error e -> print e
      _       -> return ()
    return st { currentExtra = Nothing }
  | otherwise = return st

handleResult :: GeneralResult -> Client -> State -> IO State
handleResult (Update (U.UpdateAuthorizationState { U.authorization_state = Just s })) c st
  = handleAuthState c s >> return st
handleResult (Update (U.UpdateConnectionState { U.state = Just conSt })) c st =
  handleConnState c conSt st
handleResult _ _ st = return st


emptyState :: State
emptyState = State { currentExtra     = Nothing
                   , answeringMessage = Nothing
                   , failedMessages   = []
                   , incomingMessages = []
                   , online           = False
                   }
