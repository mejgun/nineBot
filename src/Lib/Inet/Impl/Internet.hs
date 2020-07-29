module Lib.Inet.Impl.Internet
  ( newHandle
  )
where

import qualified Control.Exception             as E
import qualified Data.ByteString.Lazy          as BL
import qualified Network.HTTP.Conduit          as H

import qualified Lib.Inet                      as Inet

newHandle :: IO Inet.Handle
newHandle = return $ Inet.Handle { Inet.getPostContent = get }

get :: H.Request -> IO (Either H.HttpException BL.ByteString)
get request = do
  print request
  manager <- H.newManager H.tlsManagerSettings
  fmap (Right . H.responseBody) (H.httpLbs request manager)
    `E.catch` (return . Left)
