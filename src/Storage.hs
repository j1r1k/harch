{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Storage where

import Prelude hiding (readFile)

import Data.ByteString (ByteString)
import Data.Text (Text)

import Turtle (ExitCode, MonadIO, Line, Shell, (&))
import qualified Turtle as T (empty)
import qualified Turtle as TL (inproc, proc)
import qualified Turtle.Bytes as TB (inproc, proc)

type Path = Text

class Inproc t where
  inproc :: Text -> [Text] -> Shell t -> Shell t
  source :: Text -> [Text] -> Shell t
  source cmd args = T.empty & inproc cmd args
  target :: MonadIO io => Text -> [Text] -> Shell t -> io ExitCode

instance Inproc Line where
  inproc = TL.inproc
  target = TL.proc

instance Inproc ByteString where
  inproc = TB.inproc
  target = TB.proc

-- TODO rename
class StorageBackend s where
    removeFile :: MonadIO io => s -> Path -> io ExitCode
    exists :: MonadIO io => s -> Path -> io Bool

class (StorageBackend s) => Storage s a where
    readFile :: s -> Path -> Shell a
    writeFile :: (MonadIO io) => s -> Path -> Shell a -> io ExitCode
    readIfExists :: (MonadIO io, StorageBackend s) => s -> Path -> io (Maybe (Shell a))
    readIfExists s path = (\fe -> if fe then Just $ readFile s path else Nothing) <$> exists s path