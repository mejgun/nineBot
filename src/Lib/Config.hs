{-# LANGUAGE DeriveGeneric #-}

module Lib.Config
  ( Config(..)
  , Telegram(..)
  , Lib.Config.read
  )
where

import qualified Data.Aeson                    as A
import           Data.Maybe                     ( fromMaybe )
import           GHC.Generics                   ( Generic )

data Config =
  Config
    { telegram :: Telegram
    , logFile :: String
    }
  deriving (Generic)

data Telegram =
  Telegram
    { encryptionKey :: String
    , token :: String
    }
  deriving (Generic)

instance A.FromJSON Config

instance A.FromJSON Telegram

configFile :: String
configFile = "config.json"

read :: IO Config
read = fromMaybe (error "config parse error") <$> A.decodeFileStrict configFile
