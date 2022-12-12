module Aoc.Day.Eight.PartTwo (solvePart2) where

import Data.Text (Text)

import Aoc.Day.Eight.Common

solvePart2 :: [Text] -> Int
solvePart2 in_text = maxScore in_mat
  where in_mat = parseInput in_text
