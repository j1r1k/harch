{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE UndecidableInstances    #-}

module HArch.Configuration.Partial (
  Partial(..),
  partialToEither,
  throws,
  gParseJSONWithDefaults
) where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON(..))
import qualified Data.Aeson as A (FromArgs(..), GFromJSON, Value, Zero, gParseJSON, defaultOptions)
import qualified Data.Aeson.Types as A (Parser)
import Data.Barbie (ProductB, TraversableB)
import Data.Generic.HKD (HKD(..))
import qualified Data.Generic.HKD as HKD (HKD_, Construct, construct, fromTuple, labelsWhere, toTuple, Label, Tuple)
import Data.Maybe (isNothing)
import Data.Monoid (Last(..))

newtype Partial p = Partial { getPartial :: HKD p Last }

instance (Semigroup tuple, Generic p, HKD.Tuple Last p tuple) => Semigroup (Partial p) where
  Partial left <> Partial right = Partial $ HKD.fromTuple (HKD.toTuple left <> HKD.toTuple right)

instance (Monoid tuple, Generic p, HKD.Tuple Last p tuple) => Monoid (Partial p) where
  mempty = Partial $ HKD.fromTuple mempty

instance A.GFromJSON A.Zero (HKD.HKD_ Last p) => FromJSON (Partial p) where
  parseJSON = fmap (Partial . HKD) . A.gParseJSON A.defaultOptions A.NoFromArgs

missingFields :: 
  forall structure . 
    ( HKD.Label structure
    , ProductB (HKD structure)
    , TraversableB (HKD structure)
    ) => Partial structure -> [String]
missingFields = HKD.labelsWhere (isNothing . getLast) . getPartial

partialToEither :: 
  forall structure . 
  ( Generic structure
  , HKD.Label structure
  , ProductB (HKD structure)
  , TraversableB (HKD structure)
  , HKD.Construct Last structure
  ) => Partial structure -> Either [String] structure
partialToEither value = let missing = missingFields value in maybe (Left missing) Right (getLast $ HKD.construct $ getPartial value)

throws :: (Show e, Monad m) => Either e a -> m a
throws (Right a) = return a
throws (Left e) = fail $ show e

gParseJSONWithDefaults :: 
  forall structure tuple . 
  ( Generic structure
  , HKD.Label structure
  , ProductB (HKD structure)
  , TraversableB (HKD structure)
  , HKD.Construct Last structure
  , A.GFromJSON A.Zero (HKD.HKD_ Last structure)
  , Semigroup tuple
  , HKD.Tuple Last structure tuple
  ) => Partial structure -> A.Value -> A.Parser structure
gParseJSONWithDefaults defaults value = do
                   parsed <- parseJSON value
                   throws $ partialToEither $ defaults <> parsed