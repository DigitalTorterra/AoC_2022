module Aoc.Day.Four.PartTwo (solvePart2) where

import Data.Text (Text)

import Aoc.Day.Four.Common

bounded :: (Ord t) => t -> Range t -> Bool
bounded x (Range a b) = (a <= x) && (x <= b)

overlap :: (Ord t) => Range t -> Range t -> Bool
overlap (Range a b) (Range c d) = (bounded a (Range c d)) ||
                                  (bounded b (Range c d)) ||
                                  (bounded c (Range a b)) ||
                                  (bounded d (Range a b))

solvePart2 :: [Text] -> Int
solvePart2 in_text = length $ filter (uncurry overlap) (parseInput in_text)
