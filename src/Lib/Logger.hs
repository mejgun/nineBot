module Lib.Logger
  ( Handle(..)
  )
where

newtype Handle =
  Handle
    { logg :: String -> IO ()
    }
