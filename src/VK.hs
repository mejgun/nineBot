{-# LANGUAGE OverloadedStrings #-}

module VK
  ( parsevk
  )
where

import           Types

import qualified Text.XML.Cursor               as X
import qualified Data.Text                     as T
--import qualified Data.Text.Encoding            as E

parsevk :: X.Cursor -> Resp
parsevk c =
  Resp { video = [], photo = getImages c, caption = getTitle c, url = getURL c }

getURL :: X.Cursor -> String
getURL c =
  T.unpack
    $     T.concat
    $     c
    X.$// X.element "link"
    X.>=> X.attributeIs "rel" "canonical"
    X.>=> X.attribute "href"

getTitle :: X.Cursor -> String
getTitle c =
  T.unpack
    $     T.concat
    $     c
    X.$// X.element "div"
    X.>=> X.attributeIs "class" "wi_body"
    X.>=> X.element "div"
    X.>=> X.child
    X.>=> X.attributeIs "class" "pi_text"
    X.>=> X.child
    X.>=> X.content

getImages :: X.Cursor -> [String]
getImages c =
  map (T.unpack . fst . T.breakOn "|")
    $     c
    X.$// X.element "div"
    X.>=> (\cs ->
            (X.attributeIs "class" "wi_body" cs)
              ++ (X.attributeIs "class" "wi_body wi_no_text" cs)
          )
    X.&// X.element "div"
    X.>=> X.attributeIs "class" "thumb_map_img thumb_map_img_as_div"
    X.>=> X.attribute "data-src_big"
