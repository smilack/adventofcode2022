module AdventOfCode.Twenty22.Five where

import Prelude
import AdventOfCode.Twenty22.Util (lines)
import AdventOfCode.Twenty22.Five.Stack (Stack, push, pop, peek, empty)
import Data.Array (dropWhile, span, (!!), modifyAt, replicate, last, unsnoc)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.List (List(..), (:), fromFoldable)
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, wrap)
import Data.String (split, contains)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, runParser)
import Parsing.String (string)
import Parsing.String.Basic (intDecimal)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/Five/input"
  liftEffect do
    log "Part 1:"
    -- log ""
    -- logShow $ solve1 input
    logShow (_.init $ splitInput input)
    log "Part2:"

-- log ""
-- logShow $ solve2 input

solve :: String -> String
solve inp = execute (initializeStacks inp) (readInstructions inp)

execute :: Array (Stack String) -> List Instruction -> String
execute stacks Nil = readState stacks
execute stacks (i : is) = execute (doInst i stacks) is

doInst :: Instruction -> Array (Stack String) -> Array (Stack String)
doInst (Instruction { move, source, dest }) = go move
  where
  go :: Int -> Array (Stack String) -> Array (Stack String)
  go 0 stacks = stacks
  go n stacks = go (n - 1) (move1 stacks)

  move1 :: Array (Stack String) -> Array (Stack String)
  move1 stacks = fromMaybe stacks do
    value <- peek =<< stacks !! source
    afterPop <- modifyAt source pop stacks
    modifyAt dest (push value) afterPop

splitInput :: String -> { init :: Array String, rest :: Array String }
splitInput = lines >>> span (not <<< contains (Pattern "move"))

-- ┏━━━━━━━━┓
-- ┃ Stacks ┃
-- ┗━━━━━━━━┛

readState :: Array (Stack String) -> String
readState = foldMap (fromMaybe "" <<< peek)

initializeStacks :: String -> Array (Stack String)
initializeStacks =
  splitInput
    >>> _.init
    >>> (\_ -> [])

emptyStacks :: Int -> Array (Stack String)
emptyStacks = flip replicate empty

stackCountLine :: Array String -> String
stackCountLine inp =
  fromMaybe ""
    <<<
      ( last
          =<< map (unsnoc inp) (_.init)
      )

-- stackCountParser :: Parser String Int
-- stackCountParser = do
--   nums <- many (intDecimal =<< skipSpaces)
--   pure $ 

-- ┏━━━━━━━━━━━━━━┓
-- ┃ Instructions ┃
-- ┗━━━━━━━━━━━━━━┛

newtype Instruction = Instruction
  { move :: Int
  , source :: Int
  , dest :: Int
  }

derive instance Newtype Instruction _

derive instance Eq Instruction

instance Show Instruction where
  show (Instruction { move, source, dest }) =
    show move
      <> "×"
      <> show (source + 1)
      <> "→"
      <> show (dest + 1)

noop :: Instruction
noop = wrap { move: 0, source: 0, dest: 0 }

readInstructions :: String -> List Instruction
readInstructions =
  splitInput
    >>> _.rest
    >>> map parseInstruction
    >>> fromFoldable

parseInstruction :: String -> Instruction
parseInstruction i =
  case runParser i instructionParser of
    Left _ -> noop
    Right instruction -> instruction

-- "move 1 from 2 to 1"
instructionParser :: Parser String Instruction
instructionParser = do
  _ <- string "move "
  move <- intDecimal
  _ <- string " from "
  source <- intDecimal
  _ <- string " to "
  dest <- intDecimal
  pure $ wrap { move, source: source - 1, dest: dest - 1 }