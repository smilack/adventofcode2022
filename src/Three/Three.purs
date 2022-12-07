module AdventOfCode.Twenty22.Three where

import Prelude
import AdventOfCode.Twenty22.Three.Rucksack (Rucksack, makeSack, sharedItem, findBadge)
import Data.Array (cons, take, drop)
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
    log "Part2:"
    log "Sum of priorities of badges"
    logShow $ solve2 input

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

chunksOf :: forall a. Int -> Array a -> Array (Array a)
chunksOf _ [] = []
chunksOf n as = cons (take n as) (chunksOf n $ drop n as)

makeGroups :: Array Rucksack -> Array (Array Rucksack)
makeGroups = chunksOf 3

solve2 :: String -> Int
solve2 =
  parseInput
    >>> makeGroups
    >>> map findBadge
    >>> map prioritize
    >>> sum