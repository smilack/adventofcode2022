module Test.AdventOfCode.Twenty22.Five
  ( main
  ) where

import Prelude
import AdventOfCode.Twenty22.Five
import AdventOfCode.Twenty22.Five.Stack (Stack, push, pop, peek, empty)
import Data.List (List(..), fromFoldable)
import Data.String (split, joinWith)
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
  describe "Day Five" do
    describe "Part 1" do
      describe "Parse input" do
        it "Initialize stacks" do
          initializeStacks testIn `shouldEqual` startingStacks
        it "Extracts line with stack numbers" do
          (stackCountLine $ _.init $ splitInput testIn)
            `shouldEqual`
              " 1   2   3 "
        it "Reads instructions" do
          readInstructions testIn `shouldEqual` testInstructions
        it "Reads stack state" do
          readState startingStacks `shouldEqual` "NDP"
        it "Executes one instruction" do
          let
            s = startingStacks
            i = Instruction { move: 1, source: 1, dest: 0 }
          execute s (Cons i Nil) `shouldEqual` "DCP"
        it "Executes instructions" do
          execute startingStacks testInstructions `shouldEqual` "CMZ"
        it "Solves challenge" do
          solve testIn `shouldEqual` "CMZ"
    describe "Part 2" do
      pending "more stuff"

testIn :: String
testIn = joinWith "\n"
  [ "    [D]    "
  , "[N] [C]    "
  , "[Z] [M] [P]"
  , " 1   2   3 "
  , ""
  , "move 1 from 2 to 1"
  , "move 3 from 1 to 3"
  , "move 2 from 2 to 1"
  , "move 1 from 1 to 2"
  ]

startingStacks :: Array (Stack String)
startingStacks =
  [ push "N" $ push "Z" empty
  , push "D" $ push "C" $ push "M" empty
  , push "P" empty
  ]

testInstructions :: List Instruction
testInstructions = fromFoldable
  [ Instruction { move: 1, source: 1, dest: 0 }
  , Instruction { move: 3, source: 0, dest: 2 }
  , Instruction { move: 2, source: 1, dest: 0 }
  , Instruction { move: 1, source: 0, dest: 1 }
  ]