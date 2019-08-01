{-# LANGUAGE DuplicateRecordFields      #-}

module HArch.Configuration.Mode where

import Data.Text (Text)

import HArch.Metadata (ArchiveType)
import HArch.Path (Path(..))

data CreateArchiveConfig =
    CreateArchiveConfig {
        source :: Path,
        name :: Maybe Text,
        preferredArchiveType :: Maybe ArchiveType
    } deriving (Eq, Show)

data RestoreArchiveConfig =
    RestoreArchiveConfig {
        name :: Text,
        target :: Path
    } deriving (Eq, Show)

data HArchMode =
    CreateArchiveMode CreateArchiveConfig
  | RestoreArchiveMode RestoreArchiveConfig
  deriving (Eq, Show)