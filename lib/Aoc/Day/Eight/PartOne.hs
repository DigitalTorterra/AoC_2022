module Aoc.Day.Eight.PartOne (solvePart1) where

import Data.Text (Text)

import Aoc.Day.Eight.Common

-- Matrix Manipulation


solvePart1 :: [Text] -> Int
solvePart1 in_text = numTrue . findVisible $ in_mat
  where in_mat = parseInput in_text
