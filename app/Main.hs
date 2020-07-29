{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import qualified Lib.Bot                       as Bot
import qualified Lib.Bot.Impl.Telegram         as Bot.Impl.Telegram
import qualified Lib.Inet.Impl.Internet        as Inet.Impl.Internet
import qualified Lib.Logger.Impl.File          as Logger.Impl.File

main :: IO ()
main = do
  logH  <- Logger.Impl.File.newHandle
  inetH <- Inet.Impl.Internet.newHandle
  botH  <- Bot.Impl.Telegram.newHandle
  Bot.start botH logH inetH
