module AdventOfCode.Twenty22.Five where

import Prelude
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
  input <- readTextFile UTF8 "./src/Five/input"
  liftEffect do
    log "Part 1:"
    -- log ""
    -- logShow $ solve1 input
    log "Part2:"
    -- log ""
    -- logShow $ solve2 input