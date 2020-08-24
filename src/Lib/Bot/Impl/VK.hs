module Lib.Bot.Impl.VK
  ( newHandle
  )
where

import qualified Lib.Bot                       as Bot
import qualified Lib.Logger                    as Logger
import qualified Lib.Inet                      as Inet

newHandle :: String -> IO Bot.Handle
newHandle vkToken = return $ Bot.Handle { Bot.start = startBot vkToken }

startBot :: String -> Logger.Handle -> Inet.Handle -> IO ()
startBot vkToken logHandler inetHandler = undefined
