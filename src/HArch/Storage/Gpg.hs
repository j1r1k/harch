{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module HArch.Storage.Gpg where

import Data.ByteString (ByteString)
import Data.Text (Text)

import Turtle (Shell, (&))
import qualified Turtle.Bytes as TB (inproc)

import HArch.Storage (Storage(..))

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
  readFromFile s path = readFromFile (underlyingStorage s) path & gpgDecryptCmd
  writeToFile s path shell = shell & gpgEncryptCmd (gpgOptions s) & writeToFile (underlyingStorage s) path
  removeFile s = removeFile (underlyingStorage s)
  exists s = exists (underlyingStorage s)
