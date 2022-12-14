module AdventOfCode.Twenty22.Util
  ( lines
  ) where

import Prelude
import Data.String (split)
import Data.String.Pattern (Pattern(..))

lines :: String -> Array String
lines = split (Pattern "\n")