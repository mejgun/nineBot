module Lib.Parser where

import qualified Network.HTTP.Conduit          as H
import qualified Text.XML.Cursor               as XML

data Resp =
  Resp
    { video :: [String]
    , photo :: [String]
    , caption :: String
    , url :: String
    }
  deriving (Show)

newtype Handle =
  Handle
    { parse :: XML.Cursor -> Resp
    }
