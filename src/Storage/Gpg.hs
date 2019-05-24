{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Storage.Gpg where

import Prelude hiding (readFile, writeFile)

import Data.ByteString (ByteString)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime)
import Data.Text (Text)
import qualified Data.Text as Text (pack)

import Turtle (Line, Shell, (&))
import qualified Turtle as T (empty)
import qualified Turtle as TL (inproc)
import qualified Turtle.Bytes as TB (inproc)

import Storage (Storage(..))

data GpgOptions = GpgOptions {
    gpgSign :: Bool,
    gpgCipher :: Text,
    gpgCompress :: Text,
    gpgRecipient :: Text } deriving (Eq, Show)

gpgEncryptCmd :: GpgOptions -> Shell ByteString -> Shell ByteString
gpgEncryptCmd o = TB.inproc "gpg" args
    where signArgs = ["--sign" | gpgSign o]
          args = [ "--encrypt"
                 , "--cipher-algo" , gpgCipher o
                 , "--compress-algo", gpgCompress o
                 , "--recipient", gpgRecipient o
                 ] <> signArgs
                
gpgDecryptCmd :: Shell ByteString -> Shell ByteString
gpgDecryptCmd = TB.inproc "gpg" ["--decrypt"]

data GpgStorage s = GpgStorage { gpgOptions :: GpgOptions, underlyingStorage :: Storage s => s }

instance Storage s => Storage (GpgStorage s) where
  readFile s path = readFile (underlyingStorage s) path & gpgDecryptCmd
  writeFile s path shell = shell & gpgEncryptCmd (gpgOptions s) & writeFile (underlyingStorage s) path
  removeFile s = removeFile (underlyingStorage s)
  exists s = exists (underlyingStorage s)
