module Aoc.Day.Nine.PartTwo (solvePart2) where

import Data.Text (Text)
import qualified Data.List as L
import qualified Data.Text.IO as TIO

import Aoc.Day.Nine.Common

moveMany :: [Point] -> Direction -> [Point]
moveMany pts dir = scanl moveTail head_pos rest_list
  where head_pos = moveHead (head pts) dir
        rest_list = tail pts

getHistoryVisits :: [Direction] -> [[Point]]
getHistoryVisits in_dirs = scanl moveMany (replicate 10 (0, 0)) in_dirs

solvePart2 :: Text -> Int
solvePart2 in_text = length $ L.nub tail_visits
  where mvmts = unrollInstructions . parseLines $ in_text
        tail_visits = map last $ getHistoryVisits mvmts
