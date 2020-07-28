module Lib.Logger
  ( newHandle
  )
where

import qualified Lib.Logger                    as Logger

type Logger = String -> IO ()

newHandle :: IO Logger.Logger
newHandle = return $ Logger.Handle { logg = \_ -> return () }
