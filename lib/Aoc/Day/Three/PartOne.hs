module Aoc.Day.Three.PartOne (solvePart1) where

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Char as C


createCounts :: Text -> Map Char Int
createCounts in_text = f in_map in_str
  where in_str = T.unpack in_text
        in_map = M.empty
        f :: (Ord a) => Map a Int -> [a] -> Map a Int
        f m [] = m
        f m (c:cs) = f (M.insertWith (+) c 1 m) cs

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

splitText :: Text -> (Text, Text)
splitText t = (T.take n t, T.takeEnd n t)
  where n = T.length t `div` 2

countSides :: Text -> (Map Char Int, Map Char Int)
countSides = (mapTuple createCounts) . splitText

scoreLetter :: Char -> Int
scoreLetter c
  | C.isUpper c = (C.ord c) - (C.ord 'A') + 27
  | C.isLower c = (C.ord c) - (C.ord 'a') + 1
  | otherwise = 0

findOverlap :: (Ord k) => Map k v -> Map k v -> k
findOverlap m1 m2 = fst . head . M.toList $ ms
  where ms = M.intersection m1 m2

solvePart1 :: [Text] -> Int
solvePart1 in_lines = sum $ map (scoreLetter . (uncurry findOverlap)) maps
  where maps = map countSides in_lines
