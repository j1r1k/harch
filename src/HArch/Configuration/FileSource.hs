module HArch.Configuration.FileSource where

import Data.Aeson (FromJSON(..))
import qualified Data.Aeson as A (Value(..))
import qualified Data.HashMap.Strict as Map (fromList, keys)
import Data.Text (Text)
import qualified Data.Text as Text (pack, unpack)
import Data.Tuple.Extra (both)
import qualified Data.Yaml as YAML (decodeFileEither, parseEither)

import qualified Text.Glabrous as G (Context(..), Result(..), fromText, partialProcess')

import System.Environment (getEnvironment)

import HArch.HArchError (HArchError(..), HArchT, hoistEither, hoistEitherWith, liftIO, mapLeft)
import HArch.Path (Path, getFilePathString)
import HArch.Configuration.General (HArchConfiguration)

interpolateEnvVariablesInText :: G.Context -> Text -> Either HArchError Text
interpolateEnvVariablesInText ctx text = do
    template <- mapLeft FailedToParseTemplate $ G.fromText text
    case G.partialProcess' template ctx of
        G.Final text' -> pure text'
        G.Partial _ (G.Context context) -> Left $ FailedToInterpolateConfig $ Text.unpack <$> Map.keys context


interpolateEnvVariables :: G.Context -> A.Value -> Either HArchError A.Value
interpolateEnvVariables ctx (A.Object obj) = A.Object <$> traverse (interpolateEnvVariables ctx) obj
interpolateEnvVariables ctx (A.Array arr) = A.Array <$> traverse (interpolateEnvVariables ctx) arr
interpolateEnvVariables ctx (A.String str) = A.String <$> interpolateEnvVariablesInText ctx str
interpolateEnvVariables _ value = pure value


loadHArchConfiguration :: Path -> HArchT IO HArchConfiguration
loadHArchConfiguration path = do
    configAsValue <- liftIO $ YAML.decodeFileEither $ getFilePathString path

    configAsValue' <- hoistEitherWith (FailedToLoadConfig . show) configAsValue

    envContext <- liftIO $ G.Context . Map.fromList . fmap (both Text.pack) <$> getEnvironment

    interpolatedJson <- hoistEither $ interpolateEnvVariables envContext configAsValue'

    hoistEitherWith (FailedToLoadConfig . show) $ YAML.parseEither parseJSON interpolatedJson