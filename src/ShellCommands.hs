{-# LANGUAGE OverloadedStrings #-}

module ShellCommands where

import Data.ByteString (ByteString)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime)
import Data.Text (Text)
import qualified Data.Text as Text (pack)
import qualified Data.Text.Encoding as Text (encodeUtf8)

import Turtle (ExitCode, Line, MonadIO, Shell)
import qualified Turtle as T (empty, lineToText)
import qualified Turtle as TL (inproc)
import qualified Turtle.Bytes as TB (inproc, proc)

toNewerArgs :: Text -> Maybe LocalTime -> [Text]
toNewerArgs argName = maybe [] (\time -> [argName, Text.pack $ formatTime defaultTimeLocale "%FT%T" time])

findFilesCmd :: Text -> Maybe LocalTime -> Shell Line
findFilesCmd path newer = TL.inproc "find" args T.empty
    where newerArgs = toNewerArgs "-newermt" newer
          printfArgs = ["-printf", "{ \"path\": \"%p\", \"size\": %s, \"modified\": \"%TY-%Tm-%TdT%TH:%TM:%TS\" }\n"]
          args = [path] <> newerArgs <> printfArgs

newtype TarOptions = TarOptions { tarArgs :: [Text] } deriving (Eq, Show)

tarCreateCmd :: TarOptions -> Text -> Maybe LocalTime -> Shell ByteString
tarCreateCmd options path newer = TB.inproc "tar" args T.empty
  where newerArgs = toNewerArgs "--newer" newer
        args = ["c"] <> tarArgs options <> newerArgs <> [path]

tarExtractCmd :: MonadIO io => TarOptions -> Text -> Shell ByteString -> io ExitCode
tarExtractCmd options path = TB.proc "tar" args
  where args = ["x"] <> tarArgs options <> ["-C", path]

-- TODO append newline character
linesToBytes :: Shell Line -> Shell ByteString
linesToBytes = fmap (Text.encodeUtf8 . T.lineToText)