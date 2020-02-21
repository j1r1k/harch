{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module HArch.Storage.Gpg (
    GpgStorageConfig(..),
    GpgStorageOptions(..),
    GpgStorage(..)
) where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Semigroup (Semigroup(..))

import Turtle (Shell, (&))
import qualified Turtle.Bytes as TB (inproc)

import HArch.Storage (Storage(..), StorageConfig(..), SomeStorage(..), SomeStorageConfig(..))

data GpgStorageOptions = GpgStorageOptions {
    sign :: Bool,
    cipher :: Text,
    compress :: Text,
    recipient :: Text } deriving (Generic)

instance FromJSON GpgStorageOptions

data GpgStorageConfig = GpgStorageConfig {
    options :: GpgStorageOptions,
    underlying :: SomeStorageConfig
}

instance StorageConfig GpgStorageConfig where
    makeStorage config = SomeStorage $ GpgStorage (options (config :: GpgStorageConfig)) (makeStorage $ underlying (config :: GpgStorageConfig))

gpgEncryptCmd :: GpgStorageOptions -> Shell ByteString -> Shell ByteString
gpgEncryptCmd o = TB.inproc "gpg" args
    where signArgs = ["--sign" | sign o]
          args = [ "--encrypt"
                 , "--cipher-algo" , cipher o
                 , "--compress-algo", compress o
                 , "--recipient", recipient o
                 ] <> signArgs
                
gpgDecryptCmd :: Shell ByteString -> Shell ByteString
gpgDecryptCmd = TB.inproc "gpg" ["--decrypt"]

data GpgStorage = GpgStorage { options :: GpgStorageOptions, underlying :: SomeStorage }

instance Storage GpgStorage where
  readFromFile GpgStorage { underlying } path = readFromFile underlying path & gpgDecryptCmd
  writeToFile GpgStorage { options, underlying } path shell = shell & gpgEncryptCmd options & writeToFile underlying path
  removeFile GpgStorage { underlying } = removeFile underlying
  exists GpgStorage { underlying } = exists underlying