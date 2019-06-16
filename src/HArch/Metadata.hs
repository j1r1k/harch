{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module HArch.Metadata where

import GHC.Generics

import Data.Aeson (FromJSON(..), ToJSON(..), (.=), (.:))
import qualified Data.Aeson as A (object, withObject)
import Data.Ord (comparing)
import Data.Time.LocalTime (LocalTime)

import HArch.Path (Path)

data FileMetadata = FileMetadata {
    path :: Path,
    size :: Integer,
    modified :: LocalTime } deriving (Eq, Generic, Show)

instance ToJSON FileMetadata
instance FromJSON FileMetadata

data ArchiveType = Full | Incremental deriving (Eq, Generic, Show)

instance ToJSON ArchiveType
instance FromJSON ArchiveType

data ArchiveMetadata = ArchiveMetadata {
    archivePath :: Path,
    archiveType :: ArchiveType,
    time :: LocalTime } deriving (Eq, Show)

instance ToJSON ArchiveMetadata where
  toJSON archiveMetadata = A.object
    [ "path" .= archivePath archiveMetadata
    , "type" .= archiveType archiveMetadata
    , "time" .= time archiveMetadata
    ]
instance FromJSON ArchiveMetadata where
  parseJSON = A.withObject "ArchiveMetadata" $ \v -> ArchiveMetadata
    <$> v .: "id"
    <*> v .: "type"
    <*> v .: "time"

instance Ord ArchiveMetadata where
    compare = comparing time

data ArchiveCollection = ArchiveCollection {
    root :: Path,
    archives :: [ArchiveMetadata] } deriving (Eq, Generic, Show)

instance ToJSON ArchiveCollection
instance FromJSON ArchiveCollection

instance Ord ArchiveCollection where
    compare = comparing root