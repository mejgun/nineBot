{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( go
  )
where

import qualified Control.Exception             as E
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Network.HTTP.Conduit          as H

data Err = UnknownSite
    | NetErr H.HttpException
    | UrlErr H.HttpException
    deriving Show

data Resp = Resp
    { video   :: [String]
    , photo   :: [String]
    , caption :: String
    }
    deriving Show

type Result = Either Err Resp

go :: IO ()
go = do
  a <- getGag "https://m.9gag.com/"
  print a

getGag :: String -> IO Result
getGag s = do
  r <- parseURL s
  print r
  case r of
    Left  e   -> return $ Left $ UrlErr e
    Right req -> do
      let h    = rename $ H.host req
      let reqN = req { H.host = h }
      if h `elem` ["9gag.com", "m.vk.com"]
        then do
          res <- get9 reqN
          case res of
            Right body -> return $ Right $ parseSite h body
            Left  e    -> return $ Left $ NetErr e
        else return $ Left UnknownSite

get9 :: H.Request -> IO (Either H.HttpException BL.ByteString)
get9 request = do
  print request
  manager <- H.newManager H.tlsManagerSettings
  fmap (Right . H.responseBody) (H.httpLbs request manager)
    `E.catch` (return . Left)

parseURL :: String -> IO (Either H.HttpException H.Request)
parseURL u = fmap (Right) (H.parseRequest u) `E.catch` (return . Left)

rename :: B.ByteString -> B.ByteString
rename "m.9gag.com" = "9gag.com"
rename "vk.com"     = "m.vk.com"
rename s            = s

parseSite :: B.ByteString -> BL.ByteString -> Resp
parseSite "9gag.com" s = parse9gag s
parseSite _          _ = error "cannot match"

parse9gag :: BL.ByteString -> Resp
parse9gag _ = Resp { video = [], photo = [], caption = "ok" }
