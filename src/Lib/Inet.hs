module Lib.Inet
  ( Handle(..)
  )
where

import qualified Data.ByteString.Lazy          as BL
import qualified Network.HTTP.Conduit          as H

newtype Handle =
  Handle
    { getPostContent :: H.Request -> IO (Either H.HttpException BL.ByteString)
    }
