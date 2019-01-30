-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.EN.Rules
  ( rules ) where


import Data.Semigroup ((<>))
import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Distance.Helpers
import Duckling.Distance.Types (DistanceData(..))
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Types
import qualified Duckling.Distance.Types as TDistance
import qualified Duckling.Numeral.Types as TNumeral


distances :: [(Text, String, TDistance.Unit)]
distances = [ -- Imperial
              ("miles", "mi(le(s)?)?", TDistance.Mile)
            , ("yard", "y(ar)?ds?", TDistance.Yard)
            , ("feet", "('|f(oo|ee)?ts?)", TDistance.Foot)
            , ("inch", "(\"|''|in(ch(es)?)?)", TDistance.Inch)
              -- Metric
            , ("km", "k(ilo)?m?(et(er|re))?s?", TDistance.Kilometre)
            , ("meters", "met(er|re)s?", TDistance.Metre)
            , ("centimeters", "cm|centimet(er|re)s?", TDistance.Centimetre)
            , ("millimeters", "mm|millimet(er|re)s?", TDistance.Millimetre)
              -- Ambiguous
            , ("m (miles or meters)", "m", TDistance.M)
            ]

rulePrecision :: Rule
rulePrecision = Rule
  { name = "about|exactly <dist>"
  , pattern =
    [ regex "exactly|precisely|about|approx(\\.|imately)?|close to| near( to)?|around|almost"
    , dimension Distance
    ]
  , prod = \case
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleDistances :: [Rule]
ruleDistances = map go distances
  where
    go :: (Text, String, TDistance.Unit) -> Rule
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern = [ dimension Distance, regex regexPattern ]
      , prod = \case
          (Token Distance dd:_) -> Just . Token Distance $ withUnit u dd
          _ -> Nothing
      }

ruleIntervalBetweenNumeral :: Rule
ruleIntervalBetweenNumeral = Rule
  { name = "between|from <numeral> to|and <dist>"
  , pattern =
    [ regex "between|from"
    , Predicate isPositive
    , regex "to|and"
    , Predicate isSimpleDistance
    ]
  , prod = \case
      (_:
       Token Numeral NumeralData{TNumeral.value = from}:
       _:
       Token Distance DistanceData{TDistance.value = Just to, TDistance.unit = Just u}:
       _) | from < to ->
        Just . Token Distance . withInterval (from, to) $ unitOnly u
      _ -> Nothing
  }

ruleIntervalBetween :: Rule
ruleIntervalBetween = Rule
  { name = "between|from <dist> to|and <dist>"
  , pattern =
    [ regex "between|from"
    , Predicate isSimpleDistance
    , regex "to|and"
    , Predicate isSimpleDistance
    ]
  , prod = \case
      (_:
       Token Distance DistanceData{TDistance.value = Just from, TDistance.unit = Just u1}:
       _:
       Token Distance DistanceData{TDistance.value = Just to, TDistance.unit = Just u2}:
       _) | from < to && u1 == u2 ->
        Just . Token Distance . withInterval (from, to) $ unitOnly u1
      _ -> Nothing
  }

ruleIntervalNumeralDash :: Rule
ruleIntervalNumeralDash = Rule
  { name = "<numeral> - <dist>"
  , pattern =
    [ Predicate isPositive
    , regex "-"
    , Predicate isSimpleDistance
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = from}:
       _:
       Token Distance DistanceData{TDistance.value = Just to, TDistance.unit = Just u}:
       _) | from < to ->
         Just . Token Distance . withInterval (from, to) $ unitOnly u
      _ -> Nothing
  }

ruleIntervalDash :: Rule
ruleIntervalDash = Rule
  { name = "<dist> - <dist>"
  , pattern =
    [ Predicate isSimpleDistance
    , regex "-"
    , Predicate isSimpleDistance
    ]
  , prod = \case
      (Token Distance DistanceData{TDistance.value = Just from, TDistance.unit = Just u1}:
       _:
       Token Distance DistanceData{TDistance.value = Just to, TDistance.unit = Just u2}:
       _) | from < to && u1 == u2 ->
        Just . Token Distance . withInterval (from, to) $ unitOnly u1
      _ -> Nothing
  }

ruleIntervalMax :: Rule
ruleIntervalMax = Rule
  { name = "under/less/lower/no more than <dist>"
  , pattern =
    [ regex "under|(less|lower|not? more) than"
    , Predicate isSimpleDistance
    ]
  , prod = \case
      (_:
       Token Distance DistanceData{TDistance.value = Just to, TDistance.unit = Just u}:
       _) -> Just . Token Distance . withMax to $ unitOnly u
      _ -> Nothing
  }

ruleIntervalMin :: Rule
ruleIntervalMin = Rule
  { name = "over/above/at least/more than <dist>"
  , pattern =
    [ regex "over|above|at least|more than"
    , Predicate isSimpleDistance
    ]
  , prod = \case
      (_:
       Token Distance DistanceData{TDistance.value = Just to, TDistance.unit = Just u}:
       _) -> Just . Token Distance . withMin to $ unitOnly u
      _ -> Nothing
  }

-- | NOTE: Oxford comma is not supported.
ruleCompositeDistanceCommasAnd :: Rule
ruleCompositeDistanceCommasAnd = Rule
  { name = "composite <distance> (with ,/and)"
  , pattern =
    [ Predicate isSimpleDistance
    , regex ",|and"
    , Predicate isSimpleDistance
    ]
  , prod = \case
      (Token Distance DistanceData{TDistance.value = Just v1, TDistance.unit = Just u1}:
       _:
       Token Distance DistanceData{TDistance.value = Just v2, TDistance.unit = Just u2}:
       _) | u1 /= u2 && v1 > 0 && v2 > 0 -> Token Distance <$> maybeSummedDistance
         where
           maybeSummedDistance = fromContextualDistance $ cd1 <> cd2
           cd1 = toContextualDistance v1 u1
           cd2 = toContextualDistance v2 u2
      _ -> Nothing
  }

ruleCompositeDistance :: Rule
ruleCompositeDistance = Rule
  { name = "composite <distance>"
  , pattern =
    [ Predicate isSimpleDistance
    , Predicate isSimpleDistance
    ]
  , prod = \case
      (Token Distance DistanceData{TDistance.value = Just v1, TDistance.unit = Just u1}:
       Token Distance DistanceData{TDistance.value = Just v2, TDistance.unit = Just u2}:
       _) | u1 /= u2 && v1 > 0 && v2 > 0 -> Token Distance <$> maybeSummedDistance
         where
           maybeSummedDistance = fromContextualDistance $ cd1 <> cd2
           cd1 = toContextualDistance v1 u1
           cd2 = toContextualDistance v2 u2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleIntervalBetweenNumeral
  , ruleIntervalBetween
  , ruleIntervalMax
  , ruleIntervalMin
  , ruleIntervalNumeralDash
  , ruleIntervalDash
  , rulePrecision
  , ruleCompositeDistanceCommasAnd
  , ruleCompositeDistance
  ] ++ ruleDistances
