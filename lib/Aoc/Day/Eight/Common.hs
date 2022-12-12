module Aoc.Day.Eight.Common where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Char as C

-- Basic manipulations
findRowVisible :: [Int] -> [Bool]
findRowVisible in_list = snd $ L.mapAccumL f (-1) in_list
  where f max_height curr_height = let
          new_max_height = max max_height curr_height
          in (new_max_height, new_max_height > max_height)

findLeftVisible :: [[Int]] -> [[Bool]]
findLeftVisible = map findRowVisible

findRightVisible :: [[Int]] -> [[Bool]]
findRightVisible = rev . findLeftVisible . rev
  where rev = map L.reverse

findTopVisible :: [[Int]] -> [[Bool]]
findTopVisible = L.transpose . findLeftVisible . L.transpose

findBottomVisible :: [[Int]] -> [[Bool]]
findBottomVisible = trev . findLeftVisible . trev'
  where trev = L.transpose . (map L.reverse)
        trev' = (map L.reverse) . L.transpose

unionLists :: [[Bool]] -> [Bool]
unionLists = (map (any id)) . L.transpose

unionMats :: [[[Bool]]] -> [[Bool]]
unionMats = (map unionLists) . L.transpose

numTrue :: [[Bool]] -> Int
numTrue = sum . (map $ length. (filter id))

findVisible :: [[Int]] -> [[Bool]]
findVisible in_list = unionMats [l, r, t, b]
  where l = findLeftVisible in_list
        r = findRightVisible in_list
        t = findTopVisible in_list
        b = findBottomVisible in_list


-- More complex
type Coord = (Int, Int)

matRef :: [[a]] -> Coord -> a
matRef xs (i, j) = (xs !! i) !! j

matSize :: [[a]] -> Coord
matSize xs = (length xs, length . head $ xs)

sweepLeft :: [[a]] -> Coord -> [a]
sweepLeft xs (i, j) = map (matRef xs) pts
  where pts = reverse [(i, j') | j' <- [0..(j-1)]]

sweepRight :: [[a]] -> Coord -> [a]
sweepRight xs (i, j) = map (matRef xs) pts
  where (_sx, sy) = matSize xs
        pts = [(i, j') | j' <- [j+1..(sy-1)]]

sweepUp :: [[a]] -> Coord -> [a]
sweepUp xs (i, j) = map (matRef xs) pts
  where pts = reverse [(i', j) | i' <- [0..(i-1)]]

sweepDown :: [[a]] -> Coord -> [a]
sweepDown xs (i, j) = map (matRef xs) pts
  where (sx, _sy) = matSize xs
        pts = [(i', j) | i' <- [i+1..(sx-1)]]

scoreCoord :: [[Int]] -> Coord -> Int
scoreCoord xs cx = product num_seens
  where curr_ht = matRef xs cx
        outs = [f xs cx | f <- [sweepUp, sweepDown, sweepLeft, sweepRight]]
        num_seens = map (length . seen) outs
        seen [] = []
        seen (y:ys) = if y >= curr_ht
                         then [y]
                        else y:(seen ys)

maxScore :: [[Int]] -> Int
maxScore xs = maximum $ map (scoreCoord xs) points
  where (x, y) = matSize xs
        points = [(i, j) | i <- [0..(x-1)], j <- [0..(y-1)]]

-- Parsing
parseInput :: [Text] -> [[Int]]
parseInput = map ((map C.digitToInt) . T.unpack)
