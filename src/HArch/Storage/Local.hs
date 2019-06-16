{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module HArch.Storage.Local where

import GHC.Generics (Generic)

import Turtle (ExitCode(ExitSuccess), (&))
import qualified Turtle as T (rm, testfile)
import qualified Turtle.Bytes as TB (input, output)

import HArch.Storage (Storage(..))
import HArch.Path (Path, (</>), getFilePath)

newtype LocalStorage = LocalStorage {
  root :: Path
} deriving (Eq, Generic, Show)

instance Storage LocalStorage where
  readFromFile LocalStorage { root } path = TB.input (getFilePath $ root </> path)
  writeToFile LocalStorage { root } path input = do 
    _ <- input & TB.output (getFilePath $ root </> path)
    return ExitSuccess
  removeFile LocalStorage { root } path = do
    T.rm (getFilePath $ root </> path)
    return ExitSuccess
  exists LocalStorage { root } path = T.testfile (getFilePath $ root </> path)
