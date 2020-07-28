{-# LANGUAGE OverloadedStrings #-}

module Lib.PostContent
  ( getPost
  , Result
  , Err(..)
  , Parser.Resp(..)
  )
where

import qualified Decoder                       as D

import qualified Control.Exception             as E
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Text                     as T
import qualified Network.HTTP.Conduit          as H
import           Text.HTML.DOM                  ( parseLBS )
import qualified Text.XML.Cursor               as X

import qualified Lib.Inet                      as Inet
import qualified Lib.Parser                    as Parser
import qualified Lib.Parser.Impl.NineGag       as Parser.Impl.NineGag
import qualified Lib.Parser.Impl.VK            as Parser.Impl.VK

data Err
  = UnknownSite
  | NetErr H.HttpException
  | UrlErr H.HttpException
  deriving (Show)

type Result = Either Err Parser.Resp

getPost :: Inet.Handle -> String -> IO Result
getPost hnd str = do
  let getPostContent = Inet.getPostContent hnd
  r <- parseURL $ fixLink str
  print r
  case r of
    Left  e   -> return $ Left $ UrlErr e
    Right req -> do
      let reqN = changeHost req
      if isValidHost reqN
        then do
          res <- getPostContent reqN
          case res of
            Right body -> return $ Right $ fixCaption $ parseSite reqN body
            Left  e    -> return $ Left $ NetErr e
        else return $ Left UnknownSite

parseURL :: String -> IO (Either H.HttpException H.Request)
parseURL u = fmap Right (H.parseRequest u) `E.catch` (return . Left)

fixLink :: String -> String
fixLink s =
  let t  = T.replace "http://" "https://" . T.strip $ T.pack s
      ts = if length (T.words t) /= 1 then "fail" else t
  in  T.unpack
        $ if T.isPrefixOf "https://" ts then ts else T.concat ["https://", ts]

fixCaption :: Parser.Resp -> Parser.Resp
fixCaption r = r { Parser.caption = dec (Parser.caption r) }
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

parseSite :: H.Request -> BL.ByteString -> Parser.Resp
parseSite r b = do
  let parse9gag = Parser.parse Parser.Impl.NineGag.newHandle
      parseVK   = Parser.parse Parser.Impl.VK.newHandle
  case H.host r of
    "9gag.com" -> parse9gag $ p b
    "m.vk.com" -> parseVK $ p b
    _          -> error "cannot match"
 where
  p :: BL.ByteString -> X.Cursor
  p = X.fromDocument . parseLBS
