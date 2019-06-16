module HArch.Path where

import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as A (Value(String), withText)
import Data.String (IsString(..))
import Data.Text (Text)

import qualified Turtle as T (FilePath, (<.>), (</>), fromText)
import qualified Turtle.Format as TF (format, fp)

newtype Path = Path T.FilePath deriving (Eq, Show)

getFilePath :: Path -> T.FilePath
getFilePath (Path filePath) = filePath

getFilePathText :: Path -> Text
getFilePathText = TF.format TF.fp . getFilePath

instance FromJSON Path where
    parseJSON = A.withText "filePath" (pure . Path . T.fromText)

instance ToJSON Path where
    toJSON = A.String . getFilePathText

instance IsString Path where
    fromString = Path . fromString

instance Ord Path where
    a <= b = getFilePath a <= getFilePath b

(<.>) :: Path -> Text -> Path
path <.> extension = Path $ getFilePath path T.<.> extension

(</>) :: Path -> Path -> Path
l </> r = Path $ getFilePath l T.</> getFilePath r