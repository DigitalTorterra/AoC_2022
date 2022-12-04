module Aoc.Day.Three.PartTwo (solvePart2) where

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Char as C


coalesce :: Int -> [a] -> [[a]]
coalesce _ [] = []
coalesce k xs = (take k xs):(coalesce k (drop k xs))

createCounts :: Text -> Map Char Int
createCounts in_text = f in_map in_str
  where in_str = T.unpack in_text
        in_map = M.empty
        f :: (Ord a) => Map a Int -> [a] -> Map a Int
        f m [] = m
        f m (c:cs) = f (M.insertWith (+) c 1 m) cs

scoreLetter :: Char -> Int
scoreLetter c
  | C.isUpper c = (C.ord c) - (C.ord 'A') + 27
  | C.isLower c = (C.ord c) - (C.ord 'a') + 1
  | otherwise = 0

findOverlap :: (Ord k) => [Map k v] -> k
findOverlap ms = fst . head . M.toList $ full_intersect
  where full_intersect = foldr M.intersection (head ms) (tail ms)

solvePart2 :: [Text] -> Int
solvePart2 in_lines = sum $ map (scoreLetter . findOverlap) groups
  where groups = coalesce 3 (map createCounts in_lines)
