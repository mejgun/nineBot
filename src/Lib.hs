{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( getPost
  )
where

import           NineGag
import           Types
import           VK
import           Decoder                       as D

import qualified Control.Exception             as E
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Text                     as T
import qualified Network.HTTP.Conduit          as H
import           Text.HTML.DOM                  ( parseLBS )
import qualified Text.XML.Cursor               as X

getPost :: String -> IO Result
getPost s = do
  r <- parseURL $ fixLink s
  print r
  case r of
    Left  e   -> return $ Left $ UrlErr e
    Right req -> do
      let reqN = changeHost req
      if isValidHost reqN
        then do
          res <- get9 reqN
          case res of
            Right body -> return $ Right $ fixCaption $ parseSite reqN body
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

fixLink :: String -> String
fixLink s =
  let t  = T.replace "http://" "https://" . T.strip $ T.pack s
      ts = if length (T.words t) /= 1 then "fail" else t
  in  T.unpack
        $ if T.isPrefixOf "https://" ts then ts else T.concat ["https://", ts]

fixCaption :: Resp -> Resp
fixCaption r = r { caption = dec (caption r) }
  where dec = T.unpack . D.decode . T.pack

changeHost :: H.Request -> H.Request
changeHost r =
  let h = renameHost (H.host r)
  in  r { H.host = h, H.requestHeaders = [("User-Agent", "Mozilla")] }

renameHost :: B.ByteString -> B.ByteString
renameHost "m.9gag.com" = "9gag.com"
renameHost "vk.com"     = "m.vk.com"
renameHost s            = s

isValidHost :: H.Request -> Bool
isValidHost r = H.host r `elem` ["9gag.com", "m.vk.com"]

parseSite :: H.Request -> BL.ByteString -> Resp
parseSite r b = case H.host r of
  "9gag.com" -> parse9gag $ p b
  "m.vk.com" -> parseVK $ p b
  _          -> error "cannot match"
 where
  p :: BL.ByteString -> X.Cursor
  p = X.fromDocument . parseLBS

