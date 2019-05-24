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

class Storage s where
    readFile :: s -> Path -> Shell ByteString
    writeFile :: (MonadIO io) => s -> Path -> Shell ByteString -> io ExitCode
    removeFile :: MonadIO io => s -> Path -> io ExitCode
    exists :: MonadIO io => s -> Path -> io Bool
    readIfExists :: (MonadIO io) => s -> Path -> io (Maybe (Shell ByteString))
    readIfExists s path = (\fe -> if fe then Just $ readFile s path else Nothing) <$> exists s path