module Aoc.Day.One.Common
  (
    solveDay1
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List.Split as S

import Aoc.Day.One.PartOne (solvePart1)
import Aoc.Day.One.PartTwo (solvePart2)


-- Parse input
parseInput :: Text -> [[Int]]
parseInput txt = filter (\l -> (length l) > 0) full_list
  where split_lines = T.splitOn "\n" txt
        groups = S.splitWhen T.null split_lines
        textToInt = read . T.unpack
        full_list = map (map textToInt) groups


-- Final solution
solveDay1 :: Text -> IO (String, String)
solveDay1 puzzle_input = do
  let input = parseInput puzzle_input
      part1 = solvePart1 input
      part2 = solvePart2 input
      part1_str = show part1
      part2_str = show part2

  return (part1_str, part2_str)
