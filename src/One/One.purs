module AdventOfCode.Twenty22.One where

import Prelude
import Data.Array (take, sortBy)
import Data.Ord (compare)
import Data.Ordering (invert)
import Data.Int (fromString)
import Data.Foldable (maximum, sum)
import Data.Maybe (fromMaybe)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import PointFree ((<..))

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/One/input"
  liftEffect do
    log "Part 1:"
    -- log "Total carried amounts:"
    -- logShow $ parseInput input
    log "Largest carried amount:"
    logShow $ solve1 input
    log "Part2:"
    log "Sum of top three amounts"
    logShow $ solve2 input

solve1 :: String -> Int
solve1 =
  fromMaybe 0
    <<< maximum
    <<< map sum
    <<< parseInput

solve2 :: String -> Int
solve2 =
  sum
    <<< take 3
    <<< sortBy (invert <.. compare)
    <<< map sum
    <<< parseInput

parseInput :: String -> Array (Array Int)
parseInput = map splitFood <<< splitElves

splitElves :: String -> Array String
splitElves = split (Pattern "\n\n")

splitFood :: String -> Array Int
splitFood = map (fromMaybe 0 <<< fromString) <<< split (Pattern "\n")