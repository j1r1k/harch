{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module HArch.Storage.S3Cli (
  S3CliStorageConfig(..),
  S3CliStorage(..)
) where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON)
import Data.Text (Text)

import Turtle ((&), ExitCode(ExitSuccess))
import qualified Turtle as T (empty, proc)
import qualified Turtle.Bytes as TB (inproc, proc)

import HArch.Path (Path, getFilePathText)
import HArch.Storage (Storage(..), StorageConfig(..), SomeStorage(..))

data S3CliStorageConfig = S3CliStorageConfig {
    bucket :: Text,
    path :: Path,
    awsCliCmd :: Text,
    awsCliArgs :: [Text]
} deriving (Eq, Show, Generic)

instance StorageConfig S3CliStorageConfig where
  makeStorage = SomeStorage . S3CliStorage

instance FromJSON S3CliStorageConfig

newtype S3CliStorage = S3CliStorage S3CliStorageConfig deriving (Eq, Show)

s3Uri :: S3CliStorageConfig -> Path -> Text
s3Uri options path' = "s3://" <> bucket options <> getFilePathText (path options) <> "/" <> getFilePathText path'

instance Storage S3CliStorage where
  readFromFile (S3CliStorage options) path' = T.empty & TB.inproc (awsCliCmd options) args
    where args = ["s3", "cp"] <> awsCliArgs options <> [s3Uri options path', "-"]
  writeToFile (S3CliStorage options) path' = TB.proc (awsCliCmd options) args
    where args = ["s3", "cp"] <> awsCliArgs options <> ["-", s3Uri options path']
  removeFile (S3CliStorage options) path' = T.empty & T.proc (awsCliCmd options) args
    where args = ["s3", "rm"] <> awsCliArgs options <> [s3Uri options path']
  exists (S3CliStorage options) path' = do
    let args = ["s3", "ls"] <> awsCliArgs options <> [s3Uri options path']
    exitCode <- T.empty & T.proc (awsCliCmd options) args 
    return $ exitCode == ExitSuccess