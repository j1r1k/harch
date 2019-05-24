module ShellJSON where

import qualified Control.Foldl as Foldl (head, list)

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as A (eitherDecodeStrict)
import qualified Data.Aeson.Encode.Pretty as A (encodePretty)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BS (toStrict)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding as Text (encodeUtf8)

import Turtle (Line, MonadIO, Shell)
import qualified Turtle as T (fold, lineToText)
import qualified Turtle.Bytes as TB (strict)

eitherDecodeStrictLine :: FromJSON a => Line -> Either String a
eitherDecodeStrictLine = A.eitherDecodeStrict . Text.encodeUtf8 . T.lineToText

fromJSON :: (FromJSON a, MonadIO io) => Shell ByteString -> io (Either String a)
fromJSON shell = 
  fromMaybe (Left "Empty stream")
    <$> T.fold (A.eitherDecodeStrict
    <$> TB.strict shell) Foldl.head

fromJSONEach :: FromJSON a => Shell Line -> Shell (Either String [a])
fromJSONEach shell = let decodedLinesShell = fmap eitherDecodeStrictLine shell
    in sequence <$> T.fold decodedLinesShell Foldl.list

toJSONList :: ToJSON a => Shell a -> Shell ByteString
toJSONList shell = 
  (BS.toStrict . A.encodePretty) 
    <$> T.fold shell Foldl.list

toJSONEach :: ToJSON a => Shell a -> Shell ByteString
toJSONEach = fmap (BS.toStrict . A.encodePretty)