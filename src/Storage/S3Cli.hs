{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Storage.S3Cli where

import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Turtle ((&), ExitCode(ExitSuccess))
import qualified Turtle as T (empty, proc)

import Storage (Inproc(..), Path, Storage(..), StorageBackend(..))

data S3CliOptions = S3CliOptions {
    bucket :: Text,
    awsCliCmd :: Maybe Text,
    awsCliArgs :: [Text]
} deriving (Eq, Show)

defaultAwsCliCmd :: Text
defaultAwsCliCmd = "aws"

newtype S3CliStorage = S3CliStorage { options :: S3CliOptions } deriving (Eq, Show)

makeS3Uri :: S3CliOptions -> Path -> Path
makeS3Uri S3CliOptions { bucket } path = "s3://" <> bucket <> path

getAwsCliCmd :: S3CliOptions -> Text
getAwsCliCmd = fromMaybe defaultAwsCliCmd . awsCliCmd

instance StorageBackend S3CliStorage where
  removeFile S3CliStorage { options } path = T.empty & T.proc (getAwsCliCmd options) args
    where args = ["rm"] <> awsCliArgs options <> [makeS3Uri options path]
  exists S3CliStorage { options } path = do
    let args = ["ls"] <> awsCliArgs options <> [makeS3Uri options path]
    exitCode <- T.empty & T.proc (getAwsCliCmd options) args 
    return $ exitCode == ExitSuccess

instance (Inproc i) => Storage S3CliStorage i where
  readFile S3CliStorage { options } path = source (getAwsCliCmd options) args
    where args = ["cp"] <> awsCliArgs options <> [makeS3Uri options path, "-"]
  writeFile S3CliStorage { options } path = target (getAwsCliCmd options) args
    where args = ["cp"] <> awsCliArgs options <> ["-", makeS3Uri options path]