module AdventOfCode.Twenty22.Two where

import Prelude
import Data.Foldable (sum)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
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
  input <- readTextFile UTF8 "./src/Two/input"
  liftEffect do
    log "Part One:"
    -- log "Input:"
    -- logShow $ parseInput input
    log "Score:"
    logShow $ solve1 input
    log "Part Two:"
    -- log "Input:"
    -- logShow $ parseInput2 input
    log "Score:"
    logShow $ solve2 input

solve1 :: String -> Int
solve1 =
  sum
    <<< map score
    <<< parseInput

score :: Round -> Int
score round@{ you } = outcomeScore round + choiceScore you
  where
  outcomeScore :: Round -> Int
  outcomeScore { opponent, you }
    | you `beats` opponent = 6
    | you `eq` opponent = 3
    | otherwise = 0

choiceScore :: RPS -> Int
choiceScore = case _ of
  Rock -> 1
  Paper -> 2
  Scissors -> 3

beats :: RPS -> RPS -> Boolean
beats Rock Scissors = true
beats Scissors Paper = true
beats Paper Rock = true
beats _ _ = false

data RPS = Rock | Paper | Scissors

derive instance eqRPS :: Eq RPS
derive instance genericRPS :: Generic RPS _
instance showRPS :: Show RPS where
  show = genericShow

type Round = { opponent :: RPS, you :: RPS }

parseInput :: String -> Array Round
parseInput =
  map toRound
    <<< map (split (Pattern " "))
    <<< split (Pattern "\n")
  where
  toRound :: Array String -> Round
  toRound [ o, y ] = { opponent: toRPS o, you: toRPS y }
  toRound _ = { opponent: Rock, you: Rock }

toRPS :: String -> RPS
toRPS "A" = Rock
toRPS "B" = Paper
toRPS "C" = Scissors
toRPS "X" = Rock
toRPS "Y" = Paper
toRPS "Z" = Scissors
toRPS _ = Rock

data Outcome = Win | Loss | Draw

derive instance eqOutcome :: Eq Outcome
derive instance genericOutcome :: Generic Outcome _
instance showOutcome :: Show Outcome where
  show = genericShow

toOutcome :: String -> Outcome
toOutcome "X" = Loss
toOutcome "Y" = Draw
toOutcome "Z" = Win
toOutcome _ = Win

type Round2 = { opponent :: RPS, outcome :: Outcome }

parseInput2 :: String -> Array Round2
parseInput2 =
  map toRound2
    <<< map (split (Pattern " "))
    <<< split (Pattern "\n")

  where
  toRound2 :: Array String -> Round2
  toRound2 [ op, ou ] = { opponent: toRPS op, outcome: toOutcome ou }
  toRound2 _ = { opponent: Rock, outcome: Win }

solve2 :: String -> Int
solve2 =
  sum
    <<< map score2
    <<< parseInput2

score2 :: Round2 -> Int
score2 { opponent, outcome } =
  outcomeScore outcome + choiceScore (playerChoice opponent outcome)
  where
  outcomeScore Win = 6
  outcomeScore Draw = 3
  outcomeScore Loss = 0

  playerChoice o Draw = o
  playerChoice Rock Win = Paper
  playerChoice Rock Loss = Scissors
  playerChoice Paper Win = Scissors
  playerChoice Paper Loss = Rock
  playerChoice Scissors Win = Rock
  playerChoice Scissors Loss = Paper