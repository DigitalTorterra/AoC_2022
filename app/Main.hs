module Main where

import Control.Monad (forM_)
import Data.Text (Text)
import Input
import System.Environment (getArgs)

import Aoc.Utils (splitText)
import Aoc.Day.One.Common (solveDay1)
import Aoc.Day.Two.Common (solveDay2)
import Aoc.Day.Three.Common (solveDay3)
import qualified Aoc.Day.Four.PartOne as D4P1
import qualified Aoc.Day.Four.PartTwo as D4P2

-- NOTE: Add here the solutions for each day.
solveDay :: Int -> Text -> IO (Int, Int)
solveDay 1 input = solveDay1 input
solveDay 2 input = solveDay2 input
solveDay 3 input = solveDay3 input
solveDay 4 input = return $ (D4P1.solvePart1 $ splitText input, D4P2.solvePart2 $ splitText input)
solveDay _day _input = error "Invalid day"

solve :: Day -> IO ()
solve day = inputForDay day >>= solveDay (toInt day) >>= printSolutions day

printSolutions :: (Show a, Show b) => Day -> (a, b) -> IO ()
printSolutions day (p1, p2) = do
  putStrLn $ "Solutions for day " <> show (toInt day)
  putStrLn $ "Part I:  " <> show p1
  putStrLn $ "Part II: " <> show p2

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Missing required argument 'day:int'"
    else forM_ (parseDay $ head args) solve
