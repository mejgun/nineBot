{-# LANGUAGE OverloadedStrings #-}

module Lib.Logger
  ( newHandle
  )
where

import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.Chan
import qualified Lib.Logger                    as Logger
import qualified Lib.Constants                 as Constants

type Logger = String -> IO ()

newHandle :: IO Logger.Logger
newHandle = do
  ch <- newChan :: IO (Chan String)
  _  <- forkIO $ mainLoop ch
  return $ Logger.Handle { logg = writeChan ch }

mainLoop :: Chan String -> IO ()
mainLoop ch = do
  text <- readChan ch
  appendFile Constants.logFileName $ text ++ "\n\n"
  mainLoop ch
