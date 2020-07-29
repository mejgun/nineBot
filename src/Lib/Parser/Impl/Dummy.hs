{-# LANGUAGE OverloadedStrings #-}

module Lib.Parser.Impl.Dummy
  ( newHandle
  )
where

import qualified Text.XML.Cursor               as X

import qualified Lib.Parser                    as Parser

newHandle :: Parser.Handle
newHandle = Parser.Handle { Parser.parse = parse }

parse :: X.Cursor -> Parser.Resp
parse _ = Parser.Resp { Parser.video   = []
                      , Parser.photo   = []
                      , Parser.caption = "title"
                      , Parser.url     = "url"
                      }
