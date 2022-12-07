module AdventOfCode.Twenty22.Three.Rucksack
  ( Rucksack
  , makeSack
  , sharedItem
  , findBadge
  ) where

import Prelude
import Data.Foldable (find, elem)
import Data.Maybe (fromMaybe)
import Data.String (length, splitAt, split)
import Data.String.CodeUnits (toChar)
import Data.String.Pattern (Pattern(..))

type Compartment = String

data Rucksack = Rucksack Compartment Compartment

instance eqRucksack :: Eq Rucksack where
  eq (Rucksack a1 a2) (Rucksack b1 b2) =
    a1 == b1 && a2 == b2

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

findBadge :: Array Rucksack -> Char
findBadge [ Rucksack a1 a2, Rucksack b1 b2, Rucksack c1 c2 ] =
  fromMaybe ' ' $ toChar
    =<< find (\item -> elem item bs && elem item cs) as
  where
  as = split (Pattern "") $ a1 <> a2
  bs = split (Pattern "") $ b1 <> b2
  cs = split (Pattern "") $ c1 <> c2
findBadge _ = ' '