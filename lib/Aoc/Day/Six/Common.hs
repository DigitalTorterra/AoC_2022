module Aoc.Day.Six.Common where

import Data.List (find)

windowN :: Int -> [a] -> [[a]]
windowN n xs = if n == (length curr_view)
                  then curr_view:(windowN n (tail xs))
                else []
  where curr_view = take n xs

isUnique :: (Eq a) => [a] -> Bool
isUnique [] = True
isUnique (x:xs) = x `notElem` xs && isUnique xs

findMarker :: (Eq a) => Int -> [a] -> Maybe Int
findMarker n xs = fmap (\(i, _) -> i + n) val
  where indexed_windows = zip [0..] (windowN n xs)
        val = find (isUnique . snd) indexed_windows
