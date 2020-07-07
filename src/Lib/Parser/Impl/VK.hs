{-# LANGUAGE OverloadedStrings #-}

module Lib.Parser.Impl.VK
  ( newHandle
  )
where

import qualified Data.Text                     as T
import qualified Text.XML.Cursor               as X

import qualified Lib.Parser                    as Parser

newHandle = Parser.Handle { parse = parse }

parse :: X.Cursor -> Resp
parse c =
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
    X.$// getPostBody
    X.>=> X.child
    X.>=> X.attributeIs "class" "pi_text"
    X.>=> X.child
    X.>=> X.content

getImages :: X.Cursor -> [String]
getImages c =
  map (T.unpack . fst . T.breakOn "|")
    $     c
    X.$// getPostBody
    X.&// X.attributeIs "class" "thumb_map_img thumb_map_img_as_div"
    X.>=> X.attribute "data-src_big"

getPostBody :: X.Axis
getPostBody c =
  (X.attributeIs "class" "wi_body" c)
    ++ (X.attributeIs "class" "wi_body wi_no_text" c)
