{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Storage.Local where

import Prelude hiding (FilePath)

import Data.ByteString (ByteString)

import Turtle (ExitCode(ExitSuccess), FilePath, MonadIO, Line, Shell, (&), (</>))
import qualified Turtle as FilePath (fromText)
import qualified Turtle as T (rm, testfile)
import qualified Turtle as TL (input, output)
import qualified Turtle.Bytes as TB (input, output)

import Storage (Path, Storage(..), StorageBackend(..))

newtype LocalStorage = LocalStorage {
  root :: FilePath
} deriving (Eq, Show)

localReadFile :: (FilePath -> Shell a) -> LocalStorage -> Path -> Shell a
localReadFile input LocalStorage { root } path = input (root </> FilePath.fromText path)

localWriteFile :: MonadIO io => (FilePath -> Shell a -> io ()) -> LocalStorage -> Path -> Shell a -> io ExitCode
localWriteFile output LocalStorage { root } path input = do 
  _ <- input & output (root </> FilePath.fromText path)
  return ExitSuccess

instance StorageBackend LocalStorage where
  removeFile LocalStorage { root } path = do
    T.rm (root </> FilePath.fromText path)
    return ExitSuccess
  exists LocalStorage { root } path = T.testfile (root </> FilePath.fromText path)

instance Storage LocalStorage Line where
  readFile = localReadFile TL.input
  writeFile = localWriteFile TL.output

instance Storage LocalStorage ByteString where
  readFile = localReadFile TB.input
  writeFile = localWriteFile TB.output
