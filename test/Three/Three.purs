module Test.AdventOfCode.Twenty22.Three
  ( main
  ) where

import Prelude
import AdventOfCode.Twenty22.Three
import AdventOfCode.Twenty22.Three.Rucksack (Rucksack, sharedItem)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.QuickCheck ((===), Result)
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Day Three" do
    it "Parses input" do
      map show testRucksacks `shouldEqual` testParsedShown
    it "Finds item in both compartments" do
      map sharedItem testRucksacks `shouldEqual` testSharedItems
    it "Calculates priority correctly" do
      map prioritize testSharedItems `shouldEqual` testPriorities
    it "Calculates total priority" do
      solve1 testIn `shouldEqual` 157

testIn :: String
testIn =
  """vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"""

testRucksacks :: Array Rucksack
testRucksacks = parseInput testIn

testParsedShown :: Array String
testParsedShown =
  [ "Rucksack vJrwpWtwJgWr hcsFMMfFFhFp"
  , "Rucksack jqHRNqRjqzjGDLGL rsFMfFZSrLrFZsSL"
  , "Rucksack PmmdzqPrV vPwwTWBwg"
  , "Rucksack wMqvLMZHhHMvwLH jbvcjnnSBnvTQFn"
  , "Rucksack ttgJtRGJ QctTZtZT"
  , "Rucksack CrZsJsPPZsGz wwsLwLmpwMDw"
  ]

testSharedItems :: Array Char
testSharedItems = [ 'p', 'L', 'P', 'v', 't', 's' ]

testPriorities :: Array Int
testPriorities = [ 16, 38, 42, 22, 20, 19 ]