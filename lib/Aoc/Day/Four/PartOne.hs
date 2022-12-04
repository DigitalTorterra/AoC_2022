module Aoc.Day.Four.PartOne (solvePart1) where

import Data.Text (Text)

import Aoc.Day.Four.Common

contains :: (Ord t) => Range t -> Range t -> Bool
contains (Range a b) (Range c d) = (a <= c) && (b >= d)

contained :: (Ord t) => Range t -> Range t -> Bool
contained r1 r2 = (r1 `contains` r2) || (r2 `contains` r1)

solvePart1 :: [Text] -> Int
solvePart1 in_text = length $ filter (uncurry contained) (parseInput in_text)
