module Test.AdventOfCode.Twenty22.Two
  ( main
  ) where

import Prelude (Unit, discard, ($))
import AdventOfCode.Twenty22.Two
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Day Two" do
    describe "Part 1" do
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
    describe "Part 2" do
      it "Parses input" do
        parseInput2 testIn `shouldEqual` testParsed2
      describe "Scores games" do
        it "Draw against Rock" do
          score2 { opponent: Rock, outcome: Draw } `shouldEqual` 4
        it "Lose against Paper" do
          score2 { opponent: Paper, outcome: Loss } `shouldEqual` 1
        it "Win against Scissors" do
          score2 { opponent: Scissors, outcome: Win } `shouldEqual` 7
      it "Calculates total score" do
        solve2 testIn `shouldEqual` 12

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

testParsed2 :: Array Round2
testParsed2 =
  [ { opponent: Rock, outcome: Draw }
  , { opponent: Paper, outcome: Loss }
  , { opponent: Scissors, outcome: Win }
  ]