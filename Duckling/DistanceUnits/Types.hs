-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- | This module shadows names of some units previously defined in 'Duckling.Distance.Types'.
-- Therefore, the units from that module must be used qualified by module.
module Duckling.DistanceUnits.Types
  ( ContextualDistance (..)
  , toSystemUnit
  , toRawUnit
  ) where

import Data.Aeson
import Data.Semigroup
import Data.Tuple.Extra (both)
import Prelude
import qualified Data.Text as Text

import qualified Duckling.Distance.Types as TDistance


-- | Supports deferred resolution of ambiguous units.
-- Note that this sum type cannot be simply replaced by
-- "Maybe (ContextualDistance Double DeferrableUnit)"
-- (with "Nothing" representing "Nonrelatable").
-- See "NOTE A" below.
data ContextualDistance
  = Nonrelatable
    -- ^ If two different ambiguous units were to be composed, then
    -- it would not be possible to decide which to resolve to.
    -- As of today, this won't happen since for now "M" is the lone ambiguous unit.
  | ContextualDistance Double DeferrableUnit

-- | This represents Units that have not yet been established as unrelatable.
-- The "Definite" constructor is purposely listed in front of "Ambiguous",
-- so that it will be preferred when taking choosing the "min" between
-- an "Ambiguous" and "Definite" unit.
data DeferrableUnit
  = Definite SystemUnit
  | Ambiguous AmbiguousUnit
  deriving (Eq, Ord)

-- | These measurement-system-specific units exist to disambiguate
-- the \"M\" of the 'Unit' type and to maintain the measurement-system context
-- for resolving to the appropriate precision.
--
-- It's also just handy to have separate programmatically accessible lists
-- of units for each type of measurement system.
data SystemUnit
  = Metric MetricUnit
  | Imperial ImperialUnit
  deriving (Eq, Ord)

data ImperialUnit
  = Inch
  | Foot
  | Yard
  | Mile
  deriving (Eq, Ord)

data MetricUnit
  = Millimetre
  | Centimetre
  | Metre
  | Kilometre
  deriving (Eq, Ord)

-- | Currently there is only one actually ambiguous unit, but this design allows for expansion.
data AmbiguousUnit
  = M -- ^ "Miles" or "Metres"
  deriving (Eq, Ord)

-- | Represents a value with an unambiguous unit
data UnitValuePair = UnitValuePair SystemUnit Double

sumScaledUnits :: SystemUnit -> (UnitValuePair, UnitValuePair) -> Double
sumScaledUnits preferredUnit = uncurry (+) . both (scaleUnits preferredUnit)

-- | Determine the definite meaning of an "Ambiguous" unit using the context of
-- another "Definite" unit
reconcileAmbiguousWithDefinite :: Double -> AmbiguousUnit -> Double -> SystemUnit -> ContextualDistance
reconcileAmbiguousWithDefinite av au dv du = ContextualDistance combinedValue $ Definite preferredUnit
  where
    resolvedAmbiguousUnit = resolveUnit du au
    preferredUnit = du `min` resolvedAmbiguousUnit
    combinedValue = sumScaledUnits preferredUnit (UnitValuePair resolvedAmbiguousUnit av, UnitValuePair du dv)

-- | When both Metric and Imperial units are given, resolve to Metric.
-- Otherwise, preserve the original measurement system and use the smaller unit.
-- For the purpose of this resolution, all Metric units are considered "smaller"
-- than Imperial units.
instance Semigroup ContextualDistance where

  _ <> Nonrelatable = Nonrelatable
  Nonrelatable <> _ = Nonrelatable

  (ContextualDistance v1 u@(Ambiguous u1)) <> (ContextualDistance v2 (Ambiguous u2)) =
    if u1 == u2
    then ContextualDistance (v1 + v2) u
    else Nonrelatable -- NOTE A: Needing to return "Nonrelatable" in this edge case is why
                      -- the two-member "ContextualDistance" sum type is not exactly isophorphic
                      -- to simply wrapping the latter sum type member with Maybe.

  (ContextualDistance av (Ambiguous au)) <> (ContextualDistance dv (Definite du)) =
    reconcileAmbiguousWithDefinite av au dv du

  (ContextualDistance dv (Definite du)) <> (ContextualDistance av (Ambiguous au)) =
    reconcileAmbiguousWithDefinite av au dv du

  (ContextualDistance v1 (Definite u1)) <> (ContextualDistance v2 (Definite u2)) =
    ContextualDistance combinedValue $ Definite preferredUnit
    where
      combinedValue = sumScaledUnits preferredUnit (UnitValuePair u1 v1, UnitValuePair u2 v2)
      preferredUnit = u1 `min` u2

resolveUnit :: SystemUnit -> AmbiguousUnit -> SystemUnit
resolveUnit u = \case
  M -> case u of
    Metric _   -> Metric Metre
    Imperial _ -> Imperial Mile

-- | Disambiguation of original Unit type
toSystemUnit :: TDistance.Unit -> DeferrableUnit
toSystemUnit TDistance.M          = Ambiguous M
toSystemUnit TDistance.Millimetre = Definite $ Metric Millimetre
toSystemUnit TDistance.Centimetre = Definite $ Metric Centimetre
toSystemUnit TDistance.Metre      = Definite $ Metric Metre
toSystemUnit TDistance.Kilometre  = Definite $ Metric Kilometre
toSystemUnit TDistance.Inch       = Definite $ Imperial Inch
toSystemUnit TDistance.Foot       = Definite $ Imperial Foot
toSystemUnit TDistance.Yard       = Definite $ Imperial Yard
toSystemUnit TDistance.Mile       = Definite $ Imperial Mile

-- | Reconversion to original Unit type
toRawUnit :: DeferrableUnit -> TDistance.Unit
toRawUnit = \case
  Ambiguous au -> case au of
    M -> TDistance.M
  Definite du -> case du of
    Metric u   -> case u of
      Millimetre -> TDistance.Millimetre
      Centimetre -> TDistance.Centimetre
      Metre      -> TDistance.Metre
      Kilometre  -> TDistance.Kilometre
    Imperial u -> case u of
      Inch -> TDistance.Inch
      Foot -> TDistance.Foot
      Yard -> TDistance.Yard
      Mile -> TDistance.Mile

-- | Convert a distance to the given units.
-- This only works if the unit is unambiguous.
scaleUnits ::
     SystemUnit -- ^ target unit
  -> UnitValuePair -- ^ Original unit and value
  -> Double
scaleUnits targetUnit (UnitValuePair startingUnit v)
  | startingUnit == targetUnit = v
  | otherwise = inSIUnits v startingUnit / inSIUnits 1 targetUnit

-- | This is used when distances
-- must be normalized across measurement systems.
-- The Metric metre is the Standard International unit of distance.
inSIUnits :: Double -> SystemUnit -> Double
inSIUnits val su = case su of
  Metric u   -> inMetres u val
  Imperial u -> inInches u val * metersPerInch

-- | This conversion factor is exact.
metersPerInch :: Double
metersPerInch = 0.0254

inMetres :: Fractional a => MetricUnit -> a -> a
inMetres Millimetre n = n / 1000
inMetres Centimetre n = n / 100
inMetres Metre      n = n
inMetres Kilometre  n = n * 1000

inInches :: Num a => ImperialUnit -> a -> a
inInches Inch n = n
inInches Foot n = 12   * n
inInches Yard n = 3    * inInches Foot n
inInches Mile n = 5280 * inInches Foot n
