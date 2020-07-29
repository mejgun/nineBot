{-# LANGUAGE OverloadedStrings #-}

module Lib.Inet.Impl.Dummy
  ( newHandle
  )
where

import qualified Data.ByteString.Lazy          as BL
import qualified Network.HTTP.Conduit          as H

import qualified Lib.Inet                      as Inet

newHandle :: IO Inet.Handle
newHandle = return $ Inet.Handle { Inet.getPostContent = get }

get :: H.Request -> IO (Either H.HttpException BL.ByteString)
get request = case H.host request of
  "m.vk.com" -> Right <$> BL.readFile "test/test_page_vk.html"
  "9gag.com" -> Right <$> BL.readFile "test/test_page_9gag.html"
  _          -> return $ Left $ H.InvalidUrlException "" ""
