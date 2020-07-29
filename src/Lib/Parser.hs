module Lib.Parser where

import qualified Text.XML.Cursor               as XML

data Resp =
  Resp
    { video :: [String]
    , photo :: [String]
    , caption :: String
    , url :: String
    }
  deriving (Show, Eq)

newtype Handle =
  Handle
    { parse :: XML.Cursor -> Resp
    }
