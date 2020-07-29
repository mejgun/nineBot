module Lib.Bot
  ( Handle(..)
  )
where

import qualified Lib.Inet                      as Inet
import qualified Lib.Logger                    as Logger

newtype Handle =
  Handle
    { start :: Logger.Handle -> Inet.Handle -> IO ()
    }
