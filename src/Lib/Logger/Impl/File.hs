{-# LANGUAGE OverloadedStrings #-}

module Lib.Logger.Impl.File
  ( newHandle
  )
where

import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.Chan
import qualified Lib.Constants                 as Constants
import qualified Lib.Logger                    as Logger

newHandle :: IO Logger.Handle
newHandle = do
  ch <- newChan :: IO (Chan String)
  _  <- forkIO $ mainLoop ch
  return $ Logger.Handle { Logger.logg = writeChan ch }

mainLoop :: Chan String -> IO ()
mainLoop ch = do
  text <- readChan ch
  appendFile Constants.logFileName $ text ++ "\n\n"
  mainLoop ch
