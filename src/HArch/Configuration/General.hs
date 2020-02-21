{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module HArch.Configuration.General where

import GHC.Generics (Generic)

import Control.Lens ((&), (.~))

import Data.Aeson (FromJSON(..))
import Data.Generic.HKD (HKD(..))
import qualified Data.Generic.HKD as HKD (field)
import Data.Monoid (Last)
import Data.Text (Text)

import HArch.Configuration.Partial (Partial(..), gParseJSONWithDefaults)
import HArch.Configuration.Storage (HArchStorageConfig)
import HArch.Metadata (ArchiveType(..))
import HArch.Path (Path(..))
import HArch.ShellCommands (TarOptions(..))

data HArchGeneralConfig = 
  HArchGeneralConfig {
    storePath :: Path,
    listExtension :: Text,
    archiveExtension :: Text,
    tarOptions :: TarOptions,
    preferredArchiveType :: ArchiveType
  } deriving (Eq, Show, Generic)

defaultHArchGeneralConfig :: Partial HArchGeneralConfig
defaultHArchGeneralConfig = Partial $
  mempty @(HKD HArchGeneralConfig Last) 
    & HKD.field @"listExtension" .~ pure "list"
    & HKD.field @"archiveExtension" .~ pure "archive"
    & HKD.field @"tarOptions" .~ pure TarOptions { tarArgs = mempty }
    & HKD.field @"preferredArchiveType" .~ pure Incremental

instance FromJSON HArchGeneralConfig where
  parseJSON = gParseJSONWithDefaults defaultHArchGeneralConfig

data HArchConfiguration = HArchConfiguration {
  general :: HArchGeneralConfig,
  storage :: HArchStorageConfig
} deriving (Generic)

instance FromJSON HArchConfiguration