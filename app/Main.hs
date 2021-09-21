{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where


import qualified Lib.Bot                       as Bot
import qualified Lib.Bot.Impl.Telegram         as Bot.Impl.Telegram
import qualified Lib.Config                    as Config
import qualified Lib.Inet.Impl.Internet        as Inet.Impl.Internet
import qualified Lib.Logger.Impl.File          as Logger.Impl.File

main :: IO ()
main = do
  conf  <- Config.read
  logH  <- Logger.Impl.File.newHandle (Config.logFile conf)
  inetH <- Inet.Impl.Internet.newHandle
  botH  <- Bot.Impl.Telegram.newHandle
    (Config.token (Config.telegram conf))
    (Config.encryptionKey (Config.telegram conf))
  Bot.start botH logH inetH
