module Aoc.Day.Three.Common (solveDay3) where

import Data.Text (Text)
import qualified Data.Text as T

import Aoc.Day.Three.PartOne (solvePart1)
import Aoc.Day.Three.PartTwo (solvePart2)

-- Parse input
parseInput :: Text -> [Text]
parseInput in_text = filter (not . T.null) (T.splitOn "\n" in_text)

solveDay3 :: Text -> IO (String, String)
solveDay3 in_text = do
  let parsed_input = parseInput in_text
      part1 = solvePart1 parsed_input
      part2 = solvePart2 parsed_input
      part1_str = show part1
      part2_str = show part2

  return (part1_str, part2_str)
