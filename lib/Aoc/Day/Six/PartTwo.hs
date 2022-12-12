module Aoc.Day.Six.PartTwo (solvePart2) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromJust)

import Aoc.Day.Six.Common


solvePart2 :: Text -> Int
solvePart2 in_text = fromJust $ findMarker 14 in_str
  where in_str = T.unpack in_text
