module AdventOfCode.Twenty22.Four where

import Prelude
import AdventOfCode.Twenty22.Four.Range (Range, partialOverlap, fullOverlap, mkRange)
import Data.Enum (fromEnum)
import Data.Foldable (foldMap)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive)
import Data.Newtype (wrap, unwrap)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Typelevel.Num (d0, d1, D2)
import Data.Vec ((!!), vec2, Vec)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/Four/input"
  liftEffect do
    log "Part 1:"
    log "Number of pairs with fully overlapping range"
    logShow $ solve1 input
    log "Part2:"
    log "Number of pairs with partially overlapping range"
    logShow $ solve2 input

solve :: (Range -> Range -> Boolean) -> String -> Int
solve overlapFn =
  parseInput
    >>> foldMap (boolToAdditive <<< pairOverlaps overlapFn)
    >>> unwrap

solve2 :: String -> Int
solve2 = solve partialOverlap

solve1 :: String -> Int
solve1 = solve fullOverlap

boolToAdditive :: Boolean -> Additive Int
boolToAdditive = fromEnum >>> wrap

pairOverlaps :: (Range -> Range -> Boolean) -> Vec D2 Range -> Boolean
pairOverlaps fn v = fn (v !! d0) (v !! d1)

parseInput :: String -> Array (Vec D2 Range)
parseInput = split (Pattern "\n") >>> map mkPairs
  where
  mkPairs :: String -> Vec D2 Range
  mkPairs = split (Pattern ",") >>> unsafePartial mkVec >>> map parseRange

  mkVec :: Partial => Array String -> Vec D2 String
  mkVec [ a, b ] = vec2 a b

  parseRange :: String -> Range
  parseRange = split (Pattern "-") >>> map fromString >>> unsafePartial toRange

  toRange :: Partial => Array (Maybe Int) -> Range
  toRange [ Just a, Just b ] = mkRange a b

