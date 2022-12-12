module Aoc.Day.Twelve.Common where

import Data.Array
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Aoc.Utils

printGrid :: Array (Int, Int) Char -> IO [()]
printGrid grid = sequence $ map putStrLn $ toSimpleArray grid

toSimpleArray :: Array (Int, Int) Char -> [[Char]]
toSimpleArray grid = [[grid ! (x, y) | x<-[lowx..highx]] |  y<-[lowy..highy]]
	where ((lowx, lowy), (highx, highy)) =  bounds grid


parseInput :: Text -> Array (Int, Int) Char
parseInput in_text = array ((1, 1), (num_rows, num_cols)) out_list
  where in_lines = splitText in_text
        char_list = map T.unpack in_lines
        num_rows = length char_list
        num_cols = length . head $ char_list
        indexed_rows = zip [1..] $ map (zip [1..]) char_list
        out_list = concat $ map (\(i, xs) -> (map (\(j, x) -> ((i, j), x)) xs)) indexed_rows


main :: IO ()
main = do
  let in_path = "./puzzle-inputs/day12.txt"
  in_text <- TIO.readFile in_path

  let my_array = parseInput in_text

  printGrid my_array

  return ()


  -- putStrLn . show $ my_array
