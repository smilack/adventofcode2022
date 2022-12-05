module AdventOfCode.Twenty22.One where

import Prelude
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

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/One/input"
  liftEffect do
    log "Part 1:"
    log "Total carried amounts:"
    logShow $ parseInput input
    log "Largest carried amount:"
    logShow $ solve1 input

solve1 :: String -> Int
solve1 =
  fromMaybe 0
    <<< maximum
    <<< map sum
    <<< parseInput

parseInput :: String -> Array (Array Int)
parseInput = map splitFood <<< splitElves

splitElves :: String -> Array String
splitElves = split (Pattern "\n\n")

splitFood :: String -> Array Int
splitFood = map (fromMaybe 0 <<< fromString) <<< split (Pattern "\n")