module Lib.Logger.Impl.Dummy
  ( newHandle
  )
where

import qualified Lib.Logger                    as Logger

newHandle :: IO Logger.Handle
newHandle = return $ Logger.Handle { Logger.logg = \_ -> return () }
