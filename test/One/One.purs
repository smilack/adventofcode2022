module Test.AdventOfCode.Twenty22.One
  ( main
  ) where

import Prelude
import AdventOfCode.Twenty22.One
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
  describe "Day One" do
    it "Splits double-spaced lists" do
      splitElves testIn `shouldEqual` testSplit
    it "Parses a list of lists of numbers" do
      parseInput testIn `shouldEqual` testParsed
    it "Finds the largest-summed sublist" do
      solve1 testIn `shouldEqual` 24000

testIn :: String
testIn =
  """1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"""

testSplit :: Array (String)
testSplit =
  [ """1000
2000
3000"""
  , """4000"""
  , """5000
6000"""
  , """7000
8000
9000"""
  , """10000"""
  ]

testParsed :: Array (Array Int)
testParsed =
  [ [ 1000, 2000, 3000 ]
  , [ 4000 ]
  , [ 5000, 6000 ]
  , [ 7000, 8000, 9000 ]
  , [ 10000 ]
  ]