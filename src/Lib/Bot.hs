module Lib.Bot
  ( Handle(..)
  )
where

import qualified Lib.Logger                    as Logger
import qualified Lib.Inet                      as Inet

newtype Handle =
  Handle
    { start :: Logger.Handle->Inet.Handle->IO()
    }

