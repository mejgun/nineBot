{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( go
  )
where

import qualified Control.Exception             as E
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Text                     as T
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
  a <- getGag $ fixScheme "m.9gag.com/"
  print a

getGag :: String -> IO Result
getGag s = do
  r <- parseURL s
  print r
  case r of
    Left  e   -> return $ Left $ UrlErr e
    Right req -> do
      let reqN = changeHost req
      if isValidHost reqN
        then do
          res <- get9 reqN
          case res of
            Right body -> return $ Right $ parseSite reqN body
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

fixScheme :: String -> String
fixScheme s =
  let ts = T.replace "http://" "https://" $ T.pack s
  in  T.unpack
        $ if T.isPrefixOf "https://" ts then ts else T.concat ["https://", ts]

changeHost :: H.Request -> H.Request
changeHost r = let h = rename (H.host r) in r { H.host = h }

isValidHost :: H.Request -> Bool
isValidHost r = H.host r `elem` ["9gag.com", "m.vk.com"]

rename :: B.ByteString -> B.ByteString
rename "m.9gag.com" = "9gag.com"
rename "vk.com"     = "m.vk.com"
rename s            = s

parseSite :: H.Request -> BL.ByteString -> Resp
parseSite r b = case H.host r of
  "9gag.com" -> parse9gag b
  _          -> error "cannot match"

parse9gag :: BL.ByteString -> Resp
parse9gag _ = Resp { video = [], photo = [], caption = "ok" }
