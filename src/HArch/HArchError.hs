module HArch.HArchError (
  HArchError(..),
  HArchT,
  hoistEither,
  hoistEitherWith,
  hoistMaybe,
  liftIO,
  mapLeft,
  runHArchT
) where

import Control.Monad.Except (ExceptT(..), liftIO, runExceptT)
import Control.Error.Util (hoistEither)

import Data.Either.Extra (mapLeft, maybeToEither)

data HArchError = 
    FailedToLoadStore String 
  | FailedToLoadConfig String 
  | FailedToParseTemplate String
  | FailedToInterpolateConfig [String]
  | ArchiveNotFound String
  deriving (Eq, Show)

type HArchT m a = ExceptT HArchError m a

runHArchT :: Monad m => HArchT m a -> m (Either HArchError a)
runHArchT = runExceptT

hoistEitherWith :: Monad m => (e1 -> e2) -> Either e1 a -> ExceptT e2 m a
hoistEitherWith f = hoistEither . mapLeft f

hoistMaybe :: Monad m => e -> Maybe a -> ExceptT e m a
hoistMaybe e = hoistEither . maybeToEither e