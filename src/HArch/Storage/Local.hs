{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module HArch.Storage.Local (
  LocalStorageConfig(..),
  LocalStorage(..)
) where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON)

import Turtle (ExitCode(ExitSuccess), (&))
import qualified Turtle as T (rm, testfile)
import qualified Turtle.Bytes as TB (input, output)

import HArch.Storage (Storage(..), SomeStorage(..), StorageConfig(..))
import HArch.Path (Path, (</>), getFilePath)

data LocalStorageConfig = LocalStorageConfig { root :: Path, path :: {- Maybe -} Path } deriving (Eq, Show, Generic)

instance FromJSON LocalStorageConfig

instance StorageConfig LocalStorageConfig where
  makeStorage = SomeStorage . LocalStorage

completePath :: LocalStorage -> Path -> Path
completePath (LocalStorage LocalStorageConfig { root, path }) path' = root </> path </> path'

newtype LocalStorage = LocalStorage LocalStorageConfig deriving (Eq, Show)

instance Storage LocalStorage where
  readFromFile storage path = TB.input (getFilePath $ completePath storage path)
  writeToFile storage path input = do 
    _ <- input & TB.output (getFilePath $ completePath storage path)
    return ExitSuccess
  removeFile storage path = do
    T.rm (getFilePath $ completePath storage path)
    return ExitSuccess
  exists storage path = T.testfile (getFilePath $ completePath storage path)
