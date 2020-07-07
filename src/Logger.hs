module Logger
  ( Handle(..)
  , Logger
  )
where

type Logger = String -> IO ()

newtype Handle =
  Handle
    { logg :: Logger
    }
