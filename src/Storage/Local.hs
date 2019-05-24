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

import Storage (Path, Storage(..))

newtype LocalStorage = LocalStorage {
  root :: FilePath
} deriving (Eq, Show)

instance Storage LocalStorage where
  readFile LocalStorage { root } path = TB.input (root </> FilePath.fromText path)
  writeFile LocalStorage { root } path input = do 
    _ <- input & TB.output (root </> FilePath.fromText path)
    return ExitSuccess
  removeFile LocalStorage { root } path = do
    T.rm (root </> FilePath.fromText path)
    return ExitSuccess
  exists LocalStorage { root } path = T.testfile (root </> FilePath.fromText path)
