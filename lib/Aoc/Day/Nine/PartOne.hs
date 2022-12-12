module Aoc.Day.Nine.PartOne (solvePart1) where

import Data.Text (Text)
import qualified Data.List as L

import Aoc.Day.Nine.Common


moveBoth :: (Point, Point) -> Direction -> (Point, Point)
moveBoth (ph, pt) dir = (ph', pt')
  where ph' = moveHead ph dir
        pt' = moveTail ph' pt

getHistoryVisits :: [Direction] -> [(Point, Point)]
getHistoryVisits in_dirs = scanl moveBoth ((0, 0), (0, 0)) in_dirs

solvePart1 :: Text -> Int
solvePart1 in_text = length $ L.nub tail_visits
  where mvmts = unrollInstructions . parseLines $ in_text
        tail_visits = map snd $ getHistoryVisits mvmts
