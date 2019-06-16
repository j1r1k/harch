{-# LANGUAGE DuplicateRecordFields      #-}

module HArch.Configuration.Modes where

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
        name :: Path,
        target :: Path
    } deriving (Eq, Show)

data HArchMode =
    CreateArchiveMode CreateArchiveConfig
  | RestoreArchiveMode RestoreArchiveConfig