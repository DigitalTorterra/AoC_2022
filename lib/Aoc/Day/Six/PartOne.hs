module Aoc.Day.Six.PartOne (solvePart1) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromJust)

import Aoc.Day.Six.Common


solvePart1 :: Text -> Int
solvePart1 in_text = fromJust $ findMarker 4 in_str
  where in_str = T.unpack in_text
