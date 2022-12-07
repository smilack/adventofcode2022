module AdventOfCode.Twenty22.Three.Rucksack
  ( Rucksack
  , makeSack
  , sharedItem
  ) where

import Prelude
import Data.Foldable (find, elem)
import Data.Maybe (fromMaybe)
import Data.String (length, splitAt, split)
import Data.String.CodeUnits (toChar)
import Data.String.Pattern (Pattern(..))

type Compartment = String

data Rucksack = Rucksack Compartment Compartment

instance showRucksack :: Show Rucksack where
  show (Rucksack a b) = "Rucksack " <> a <> " " <> b

makeSack :: String -> Rucksack
makeSack s = make $ splitAt (length s / 2) s
  where
  make { before, after } = Rucksack before after

sharedItem :: Rucksack -> Char
sharedItem (Rucksack a b) =
  fromMaybe ' ' $ toChar =<< find (_ `elem` bs) as
  where
  as = split (Pattern "") a
  bs = split (Pattern "") b
