module AdventOfCode.Twenty22.Three where

import Prelude
import AdventOfCode.Twenty22.Three.Rucksack (Rucksack, makeSack, sharedItem)
import Data.Char (toCharCode)
import Data.CodePoint.Unicode (isUpper)
import Data.Foldable (sum)
import Data.String (split)
import Data.String.CodePoints (codePointFromChar)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/Three/input"
  liftEffect do
    log "Part 1:"
    log "Sum of priorities of duplicated items:"
    logShow $ solve1 input

-- log "Part2:"
-- log ""
-- logShow $ solve2 input

solve1 :: String -> Int
solve1 =
  parseInput
    >>> map sharedItem
    >>> map prioritize
    >>> sum

parseInput :: String -> Array Rucksack
parseInput = map makeSack <<< split (Pattern "\n")

prioritize :: Char -> Int
prioritize c =
  if isUpper (codePointFromChar c) then
    code - bigA + 27
  else
    code - a + 1
  where
  code = toCharCode c
  a = toCharCode 'a'
  bigA = toCharCode 'A'