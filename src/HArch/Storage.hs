{-# LANGUAGE ExistentialQuantification #-}

module HArch.Storage where

import Data.ByteString (ByteString)

import Turtle (ExitCode, MonadIO, Shell)

import HArch.Path (Path)

class Storage s where
    readFromFile :: s -> Path -> Shell ByteString
    writeToFile :: (MonadIO io) => s -> Path -> Shell ByteString -> io ExitCode
    removeFile :: MonadIO io => s -> Path -> io ExitCode
    exists :: MonadIO io => s -> Path -> io Bool
    readIfExists :: (MonadIO io) => s -> Path -> io (Maybe (Shell ByteString))
    readIfExists s path = (\fe -> if fe then Just $ readFromFile s path else Nothing) <$> exists s path

data SomeStorage = forall s . Storage s => SomeStorage s

instance Storage SomeStorage where
  readFromFile (SomeStorage s) = readFromFile s
  writeToFile (SomeStorage s) = writeToFile s
  removeFile (SomeStorage s) = removeFile s
  exists (SomeStorage s) = exists s

class StorageConfig o where
    makeStorage :: o -> SomeStorage

data SomeStorageConfig = forall o . StorageConfig o => SomeStorageConfig o

instance StorageConfig SomeStorageConfig where
    makeStorage (SomeStorageConfig underlying) = makeStorage underlying