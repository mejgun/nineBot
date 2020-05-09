{-# LANGUAGE OverloadedStrings #-}

module NineGag
  ( parse9gag
  )
where

import           Types

import           Data.Maybe                     ( fromMaybe )
import qualified Text.XML.Cursor               as X
import qualified Data.Text                     as T
import qualified Data.Aeson                    as A
import qualified Data.Text.Encoding            as E

parse9gag :: X.Cursor -> Resp
parse9gag c = Resp { video   = getVideo j
                   , photo   = getImage j
                   , caption = getTitle j
                   , url     = getURL j
                   }
  where j = getJson c

getTitle :: T.Text -> String
getTitle json = let Title q = fromMaybe (Title "title parse failed") t in q
  where t = dec json :: Maybe Title

getURL :: T.Text -> String
getURL json = let URL q = fromMaybe (URL "https://google.com") t in q
  where t = dec json :: Maybe URL

getVideo :: T.Text -> [String]
getVideo json = case t of
  Just (Video s) -> [s]
  _              -> []
  where t = dec json :: Maybe Video

getImage :: T.Text -> [String]
getImage json = case t of
  Just (Image s) -> [s]
  _              -> []
  where t = dec json :: Maybe Image

dec :: A.FromJSON a => T.Text -> Maybe a
dec = A.decodeStrict . E.encodeUtf8

getJson :: X.Cursor -> T.Text
getJson c = case p of
  [x] -> T.replace "\\" "" . strip2 $ strip1 x
  _   -> ""
 where
  p =
    filter (\s -> T.isPrefixOf prefix s && T.isSuffixOf suffix s)
      $     map (T.strip)
      $     c
      X.$// X.element "script"
      X.>=> X.attributeIs "type" "text/javascript"
      X.>=> X.child
      X.>=> X.content

prefix :: T.Text
prefix = "window._config = JSON.parse(\""

suffix :: T.Text
suffix = "\");"

strip1 :: T.Text -> T.Text
strip1 s = let Just x = T.stripPrefix prefix s in x

strip2 :: T.Text -> T.Text
strip2 s = let Just x = T.stripSuffix suffix s in x

data Title = Title String
instance A.FromJSON Title where
  parseJSON = A.withObject "title" $ \o -> do
    d <- o A..: "data"
    p <- d A..: "post"
    t <- p A..: "title"
    return $ Title t

data URL = URL String
instance A.FromJSON URL where
  parseJSON = A.withObject "url" $ \o -> do
    d <- o A..: "data"
    p <- d A..: "post"
    t <- p A..: "url"
    return $ URL t

data Video = Video String
instance A.FromJSON Video where
  parseJSON = A.withObject "video" $ \o -> do
    d <- o A..: "data"
    p <- d A..: "post"
    i <- p A..: "images"
    w <- i A..: "image460sv"
    t <- w A..: "url"
    return $ Video t

data Image = Image String
instance A.FromJSON Image where
  parseJSON = A.withObject "image" $ \o -> do
    d <- o A..: "data"
    p <- d A..: "post"
    i <- p A..: "images"
    w <- mconcat [i A..: "image700", i A..: "image460"]
    t <- w A..: "url"
    return $ Image t
