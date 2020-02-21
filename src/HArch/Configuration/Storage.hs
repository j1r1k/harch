{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module HArch.Configuration.Storage (
  StorageType(..),
  PartialStorageConfig(..),
  HArchStorageConfig(..)
) where

import GHC.Generics (Generic)

import Control.Lens ((&), (.~))

import Data.Aeson (FromJSON(..), (.:), (.!=))
import Data.Aeson.Types (Parser, Value)
import qualified Data.Aeson as A (withObject, withText)
import Data.Generic.HKD (HKD(..))
import qualified Data.Generic.HKD as HKD (field)
import Data.Monoid (Last)

import HArch.Configuration.Partial (Partial(..), gParseJSONWithDefaults)
import HArch.Storage (SomeStorageConfig(..))
import HArch.Storage.Local (LocalStorageConfig(..))
import HArch.Storage.Mirror (MirrorStorageConfig(..))
import HArch.Storage.Gpg (GpgStorageConfig(..), GpgStorageOptions(..))
import HArch.Storage.S3Cli (S3CliStorageConfig(..))

data StorageType = 
    Local 
  | S3Cli 
  | Mirror 
  | Gpg 
  deriving (Eq, Show)

instance FromJSON StorageType where
    parseJSON = A.withText "StorageType" $ 
                  \case "local" -> pure Local
                        "s3cli" -> pure S3Cli
                        "mirror" -> pure Mirror
                        "gpg" -> pure Gpg
                        _ -> fail "unsupported StorageType"

storageOptionsParser :: PartialStorageConfig -> StorageType -> Value -> Parser SomeStorageConfig
storageOptionsParser defaults Local value = 
  SomeStorageConfig <$> gParseJSONWithDefaults (local defaults) value
storageOptionsParser defaults S3Cli value =
  SomeStorageConfig <$> gParseJSONWithDefaults (s3cli defaults) value
storageOptionsParser defaults Gpg value = (A.withObject "GpgStorageOptions" $ \o -> do
  underlying' <- o .: "underlying" >>= someStorageConfigParser defaults
  optionsWithDefaults <- gParseJSONWithDefaults (gpg defaults) value
  return $ SomeStorageConfig $ GpgStorageConfig optionsWithDefaults underlying') value
storageOptionsParser defaults Mirror value = parser value
  where parser = A.withObject "MirrorStorageOptions" $ \o -> do 
          underlying' <- o .: "underlying" >>= mapM (someStorageConfigParser defaults) 
          return $ SomeStorageConfig $ MirrorStorageConfig underlying'

someStorageConfigParser :: PartialStorageConfig -> Value -> Parser SomeStorageConfig
someStorageConfigParser defaults value = parser value
      where parser = A.withObject "SomeStorageConfig" $ \o -> do
              type' <- o .: "type"
              storageOptionsParser defaults type' value

data PartialStorageConfig = PartialStorageConfig {
  gpg :: Partial GpgStorageOptions,
  local :: Partial LocalStorageConfig,
  s3cli :: Partial S3CliStorageConfig
} deriving (Generic)

instance FromJSON PartialStorageConfig

instance Semigroup PartialStorageConfig where
  left <> right = PartialStorageConfig {
    gpg = gpg left <> gpg right,
    local = local left <> local right,
    s3cli = s3cli left <> s3cli right
  }

instance Monoid PartialStorageConfig where
  mempty = PartialStorageConfig {
    gpg = mempty,
    local = mempty,
    s3cli = mempty
  }

data HArchStorageConfig = HArchStorageConfig {
  store :: SomeStorageConfig,
  lists :: SomeStorageConfig,
  files :: SomeStorageConfig
}

defaultGpgStorageOptions :: Partial GpgStorageOptions
defaultGpgStorageOptions = Partial $
  mempty @(HKD GpgStorageOptions Last) 
    & HKD.field @"sign" .~ pure True
    & HKD.field @"cipher" .~ pure "AES256"
    & HKD.field @"compress" .~ pure "BZIP2"

defaultS3CliStorageConfig :: Partial S3CliStorageConfig
defaultS3CliStorageConfig = Partial $
  mempty @(HKD S3CliStorageConfig Last)
    & HKD.field @"awsCliCmd" .~ pure "aws"
    & HKD.field @"awsCliArgs" .~ pure mempty

defaultStorageConfig :: PartialStorageConfig
defaultStorageConfig = PartialStorageConfig {
  gpg = defaultGpgStorageOptions,
  local = mempty,
  s3cli = defaultS3CliStorageConfig
}

instance FromJSON HArchStorageConfig where
  parseJSON = A.withObject "StorageConfig" $ \o -> do
    defaults' <- o .: "defaults" .!= mempty
    let combinedDefaults = defaultStorageConfig <> defaults'
    store' <- o .: "store" >>= someStorageConfigParser combinedDefaults 
    lists' <- o .: "lists" >>= someStorageConfigParser combinedDefaults
    files' <- o .: "files" >>= someStorageConfigParser combinedDefaults

    return HArchStorageConfig {
      store = store',
      lists = lists',
      files = files'
    }