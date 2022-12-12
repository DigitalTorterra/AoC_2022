module Aoc.Day.Seven.PartTwo (solvePart2) where

import Data.Text (Text)

import Aoc.Day.Seven.Common


-- Constants
total_size :: Int
total_size = 70000000

target_unused_space :: Int
target_unused_space = 30000000

allDirSizes :: Inode -> [Int]
allDirSizes curr = case curr of
  (File _ _) -> []
  (Directory _ is) -> [inodeSize curr] ++ (concat $ map allDirSizes is)


solvePart2 :: Text -> Int
solvePart2 in_text = minimum $ filter (>= deletion_margin) all_sizes
  where root_dir = parseInput in_text
        all_sizes = allDirSizes root_dir
        curr_used_space = inodeSize root_dir
        curr_unused_space = total_size - curr_used_space
        deletion_margin = target_unused_space - curr_unused_space
