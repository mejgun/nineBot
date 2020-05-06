module Types where

import qualified Network.HTTP.Conduit          as H

data Err = UnknownSite
    | NetErr H.HttpException
    | UrlErr H.HttpException
    deriving Show

data Resp = Resp
    { video   :: [String]
    , photo   :: [String]
    , caption :: String
    , url :: String
    }
    deriving Show

type Result = Either Err Resp
