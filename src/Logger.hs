{-# LANGUAGE OverloadedStrings #-}

module Logger
  ( startLogger
  , Logger
  )
where

import           Control.Concurrent.Chan
import           Control.Concurrent             ( forkIO )

type Logger = String -> IO ()

startLogger :: IO (Logger)
startLogger = do
  ch <- newChan :: IO (Chan String)
  _  <- forkIO $ mainLoop ch
  return $ logToFile ch

logToFile :: Chan String -> String -> IO ()
logToFile ch s = writeChan ch s

logFileName :: FilePath
logFileName = "bot_log.txt"

mainLoop :: Chan String -> IO ()
mainLoop ch = do
  text <- readChan ch
  appendFile logFileName text
  appendFile logFileName "\n\n"
  mainLoop ch