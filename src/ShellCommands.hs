{-# LANGUAGE OverloadedStrings #-}

module ShellCommands (
  GpgOptions(..),
  TarOptions(..),
  findFilesCmd,
  tarCreateCmd,
  tarExtractCmd,
  gpgEncryptCmd,
  gpgDecryptCmd
) where

import Data.ByteString (ByteString)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime)
import Data.Text (Text)
import qualified Data.Text as Text (pack)

import Turtle (Line, Shell)
import qualified Turtle as T (empty)
import qualified Turtle as TL (inproc)
import qualified Turtle.Bytes as TB (inproc)

toNewerArgs :: Text -> Maybe LocalTime -> [Text]
toNewerArgs argName = maybe [] (\time -> [argName, Text.pack $ formatTime defaultTimeLocale "%FT%T" time])

findFilesCmd :: Text -> Maybe LocalTime -> Shell Line
findFilesCmd path newer = TL.inproc "find" args T.empty
    where newerArgs = toNewerArgs "-newermt" newer
          printfArgs = ["-printf", "{ \"path\": \"%p\", \"size\": %s, \"modified\": \"%TY-%Tm-%TdT%TH:%TM:%TS\" }\n"]
          args = [path] <> newerArgs <> printfArgs

data GpgOptions = EncryptOptions {
    gpgSign :: Bool,
    gpgCipher :: Text,
    gpgCompress :: Text,
    gpgRecipient :: Text } deriving (Eq, Show)

gpgEncryptCmd :: GpgOptions -> Shell ByteString -> Shell ByteString
gpgEncryptCmd options = TB.inproc "gpg" args
    where signArgs = ["--sign" | gpgSign options]
          args = [ "--encrypt"
                 , "--cipher-algo" , gpgCipher options
                 , "--compress-algo", gpgCompress options
                 , "--recipient", gpgRecipient options
                 ] <> signArgs
                
gpgDecryptCmd :: Shell ByteString -> Shell ByteString
gpgDecryptCmd = TB.inproc "gpg" ["--decrypt"]

newtype TarOptions = TarOptions { tarArgs :: [Text] } deriving (Eq, Show)

tarCreateCmd :: TarOptions -> Text -> Maybe LocalTime -> Shell ByteString
tarCreateCmd options path newer = TB.inproc "tar" args T.empty
  where newerArgs = toNewerArgs "--newer" newer
        args = ["c"] <> tarArgs options <> newerArgs <> [path]

tarExtractCmd :: TarOptions -> Text -> Shell ByteString -> Shell ByteString
tarExtractCmd options path = TB.inproc "tar" args
  where args = ["x"] <> tarArgs options <> ["-C", path]