module HArch.Storage.Mirror where

import Control.Monad.Extra (ifM)

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty (toList, init, last)
import Data.Monoid (Any(..), First(..))

import HArch.CombinedExitCode (CombinedExitCode(..))
import HArch.Path (Path)
import HArch.Storage (Storage(..), StorageConfig(..), SomeStorage(..), SomeStorageConfig(..))

import Turtle (Shell)

newtype MirrorStorageConfig = MirrorStorageConfig (NonEmpty SomeStorageConfig)

instance StorageConfig MirrorStorageConfig where
  makeStorage (MirrorStorageConfig underlyings) = SomeStorage $ MirrorStorage $ makeStorage <$> underlyings

newtype MirrorStorage = MirrorStorage (NonEmpty SomeStorage)

readFromFirstAvailable :: ([SomeStorage], SomeStorage) -> Path -> Shell ByteString
readFromFirstAvailable ([], lastStorage) path = readFromFile lastStorage path
readFromFirstAvailable (s1:storages, lastStorage) path = ifM (exists s1 path) (readFromFile s1 path) (readFromFirstAvailable (storages, lastStorage) path)

-- TODO improve
instance Storage MirrorStorage where
  readFromFile (MirrorStorage underlyings) = readFromFirstAvailable (NonEmpty.init underlyings, NonEmpty.last underlyings)
  writeToFile (MirrorStorage underlyings) path input = do
    codes <- mapM (\storage -> CombinedExitCode <$> writeToFile storage path input) $ NonEmpty.toList underlyings
    return $ unwrapExitCode $ mconcat codes
  removeFile (MirrorStorage underlyings) path = do
    codes <- mapM (\storage -> CombinedExitCode <$> removeFile storage path) $ NonEmpty.toList underlyings
    return $ unwrapExitCode $ mconcat codes
  exists (MirrorStorage underlyings) path = do
    anys <- mapM (\storage -> Any <$> exists storage path) $ NonEmpty.toList underlyings
    return $ getAny $ mconcat anys
  readIfExists (MirrorStorage underlyings) path = do
    firsts <- mapM (\storage -> First <$> readIfExists storage path) $ NonEmpty.toList underlyings
    return $ getFirst $ mconcat firsts