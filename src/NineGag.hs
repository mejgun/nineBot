{-# LANGUAGE OverloadedStrings #-}

module NineGag
  ( parse9gag
  )
where

import           Types

import qualified Text.XML.Cursor               as X
import qualified Data.Text                     as T
--import qualified Data.Aeson                    as A


parse9gag :: X.Cursor -> Resp
parse9gag c = Resp
  { video   = []
  , photo   = []
  , caption = T.unpack
              $     T.intercalate "\n\n"
              $     filter (T.isPrefixOf "window._config = JSON.parse(")
              $     c
              X.$// X.element "script"
              X.>=> X.attributeIs "type" "text/javascript"
              X.>=> X.child
              X.>=> X.content
  , url     = ""
  --  X.>=> X.element "header" 
              --X.>=> X.element "h1"
  }


-- /html/body/div[2]/div[1]/div[2]/div[1]/section[1]/article/header/h1
-- /html/body/div[2]/div[1]/div[2]/div[1]/section[1]/article/div[2]/div[1]/a/div/picture/img
