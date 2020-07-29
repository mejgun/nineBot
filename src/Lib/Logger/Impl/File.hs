{-# LANGUAGE OverloadedStrings #-}

module Lib.Logger.Impl.File
  ( newHandle
  )
where

import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.Chan
import qualified Lib.Logger                    as Logger

newHandle :: FilePath -> IO Logger.Handle
newHandle file = do
  ch <- newChan :: IO (Chan String)
  _  <- forkIO $ mainLoop file ch
  return $ Logger.Handle { Logger.logg = writeChan ch }

mainLoop :: FilePath -> Chan String -> IO ()
mainLoop file ch = do
  text <- readChan ch
  appendFile file $ text ++ "\n\n"
  mainLoop file ch
