module Aoc.Day.Seven.PartOne (solvePart1) where

import Data.Text (Text)

import Aoc.Day.Seven.Common

gatedSize :: Inode -> Int
gatedSize curr = if curr_size <= 100000
                    then curr_size
                 else 0
  where curr_size = inodeSize curr

sumDirSizes :: Inode -> Int
sumDirSizes curr = case curr of
  (File _ _) -> 0
  (Directory _ is) -> (gatedSize curr) + (sum $ map sumDirSizes is)

-- unrollDirSizes :: Inode -> [Int]
-- unrollDirSizes (File _ sz) = []
-- unrollDirSizes (Directory _ is) = undefined

  -- where
  --   -- Separate out files and directories
  --   all_files = filter isFile is
  --   all_dirs = filter isDirectory is

  --   -- Get the file sizes
  --   file_sizes = sum $ map (\(File _ sz) -> sz) all_files

  --   -- Get directory sizes
  --   dir_sizes = map unrollDirSizes all_dirs
  --   total_dir_sizes =


solvePart1 :: Text -> Int
solvePart1 in_text = sumDirSizes root_dir
  where root_dir = parseInput in_text
