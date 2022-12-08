module Test.AdventOfCode.Twenty22.Four
  ( main
  ) where

import Prelude (Unit, discard, ($))
import AdventOfCode.Twenty22.Four
import AdventOfCode.Twenty22.Four.Range (Range, mkRange, partialOverlap, fullOverlap)
import Data.Typelevel.Num (D2)
import Data.Vec (vec2, Vec)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Day Four" do
    describe "Part 1" do
      it "Parses input into ranges" do
        parseInput testIn `shouldEqual` testParsed
      it "Determines if a range is fully contained by another" do
        fullOverlap (mkRange 2 4) (mkRange 6 8) `shouldEqual` false
        fullOverlap (mkRange 2 3) (mkRange 4 5) `shouldEqual` false
        fullOverlap (mkRange 5 7) (mkRange 7 9) `shouldEqual` false
        fullOverlap (mkRange 2 8) (mkRange 3 7) `shouldEqual` true
        fullOverlap (mkRange 6 6) (mkRange 4 6) `shouldEqual` true
        fullOverlap (mkRange 2 6) (mkRange 4 8) `shouldEqual` false
      it "Counts pairs that fully overlap" do
        solve1 testIn `shouldEqual` 2
    describe "Part 2" do
      it "Determines if ranges partially overlap" do
        partialOverlap (mkRange 2 4) (mkRange 6 8) `shouldEqual` false
        partialOverlap (mkRange 2 3) (mkRange 4 5) `shouldEqual` false
        partialOverlap (mkRange 5 7) (mkRange 7 9) `shouldEqual` true
        partialOverlap (mkRange 2 8) (mkRange 3 7) `shouldEqual` true
        partialOverlap (mkRange 6 6) (mkRange 4 6) `shouldEqual` true
        partialOverlap (mkRange 2 6) (mkRange 4 8) `shouldEqual` true
      it "Counts pairs that partially overlap" do
        solve2 testIn `shouldEqual` 4

testIn :: String
testIn =
  """2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"""

testParsed :: Array (Vec D2 Range)
testParsed =
  [ vec2 (mkRange 2 4) (mkRange 6 8)
  , vec2 (mkRange 2 3) (mkRange 4 5)
  , vec2 (mkRange 5 7) (mkRange 7 9)
  , vec2 (mkRange 2 8) (mkRange 3 7)
  , vec2 (mkRange 6 6) (mkRange 4 6)
  , vec2 (mkRange 2 6) (mkRange 4 8)
  ]

