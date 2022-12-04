module Aoc.Day.Two.Common (solveDay2) where

import Data.Text (Text)
import qualified Data.Text as T

import Aoc.Day.Two.PartOne (solvePart1)
import Aoc.Day.Two.PartTwo (solvePart2)

parseText :: Text -> [(Char, Char)]
parseText in_text = map (\w -> (T.head w, T.last w)) non_null
  where in_lines = T.splitOn "\n" in_text
        non_null = filter (not . T.null) in_lines


solveDay2 :: Text -> IO (Int, Int)
solveDay2 in_text = do
  let moves = parseText in_text
      res1 = solvePart1 moves
      res2 = solvePart2 moves
  return (res1, res2)
