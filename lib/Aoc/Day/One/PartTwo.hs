module Aoc.Day.One.PartTwo
  (
    solvePart2
  )
where

import Data.List (sort)

-- Helper functions
topK :: (Ord a) => Int -> [a] -> [a]
topK k = (take k) . reverse . sort

-- Solution
solvePart2 :: [[Int]] -> Int
solvePart2 = sum . (topK 3) . (map sum)
