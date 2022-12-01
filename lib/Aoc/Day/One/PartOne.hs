module Aoc.Day.One.PartOne where

solvePart1 :: [[Int]] -> Int
solvePart1 = maximum . (map sum)
