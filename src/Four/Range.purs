module AdventOfCode.Twenty22.Four.Range
  ( Range
  , mkRange
  , fullOverlap
  , partialOverlap
  ) where

import Prelude

data Range = Range Int Int

instance showRange :: Show Range where
  show (Range a b) = show a <> "-" <> show b

instance eqRange :: Eq Range where
  eq (Range a1 b1) (Range a2 b2) = a1 == a2 && b1 == b2

mkRange :: Int -> Int -> Range
mkRange a b = Range (min a b) (max a b)

subrangeOf :: Range -> Range -> Boolean
subrangeOf inner outer =
  low inner >= low outer
    && high inner <= high outer

low :: Range -> Int
low (Range a _) = a

high :: Range -> Int
high (Range _ b) = b

fullOverlap :: Range -> Range -> Boolean
fullOverlap a b = a `subrangeOf` b || b `subrangeOf` a

partialOverlap :: Range -> Range -> Boolean
partialOverlap a b =
  (high a >= low b && high a <= high b)
    || (low a >= low b && low a <= high b)
    || (high b >= low a && high b <= high a)
    || (low b >= low a && low b <= high a)