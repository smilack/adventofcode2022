module Test.AdventOfCode.Twenty22.Two
  ( main
  ) where

import Prelude
import AdventOfCode.Twenty22.Two
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
  describe "Day Two" do
    it "Parses input" do
      parseInput testIn `shouldEqual` testParsed
    describe "Scores games" do
      it "Paper against Rock" do
        score { opponent: Rock, you: Paper } `shouldEqual` 8
      it "Rock against Paper" do
        score { opponent: Paper, you: Rock } `shouldEqual` 1
      it "Scissors against Scissors" do
        score { opponent: Scissors, you: Scissors } `shouldEqual` 6
    it "Calculates total score" do
      solve1 testIn `shouldEqual` 15

testIn :: String
testIn =
  """A Y
B X
C Z"""

testParsed :: Array Round
testParsed =
  [ { opponent: Rock, you: Paper }
  , { opponent: Paper, you: Rock }
  , { opponent: Scissors, you: Scissors }
  ]