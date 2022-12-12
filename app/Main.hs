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
import qualified Aoc.Day.Five.PartOne as D5P1
import qualified Aoc.Day.Five.PartTwo as D5P2
import qualified Aoc.Day.Six.PartOne as D6P1
import qualified Aoc.Day.Six.PartTwo as D6P2
import qualified Aoc.Day.Seven.PartOne as D7P1
import qualified Aoc.Day.Seven.PartTwo as D7P2
import qualified Aoc.Day.Eight.PartOne as D8P1
import qualified Aoc.Day.Eight.PartTwo as D8P2
import qualified Aoc.Day.Nine.PartOne as D9P1
import qualified Aoc.Day.Nine.PartTwo as D9P2
import qualified Aoc.Day.Ten.PartOne as D10P1
import qualified Aoc.Day.Ten.PartTwo as D10P2
import qualified Aoc.Day.Eleven.PartOne as D11P1
import qualified Aoc.Day.Eleven.PartTwo as D11P2
import qualified Aoc.Day.Twelve.PartOne as D12P1
import qualified Aoc.Day.Twelve.PartTwo as D12P2

-- NOTE: Add here the solutions for each day.
solveDay :: Int -> Text -> IO (String, String)
solveDay 1 input = solveDay1 input
solveDay 2 input = solveDay2 input
solveDay 3 input = solveDay3 input
solveDay 4 input = return $ (show . D4P1.solvePart1 $ splitText input,
                             show . D4P2.solvePart2 $ splitText input)
solveDay 5 input = return $ (D5P1.solvePart1 input, D5P2.solvePart2 $ input)
solveDay 6 input = return $ (show $ D6P1.solvePart1 input, show $ D6P2.solvePart2 input)
solveDay 7 input = return $ (show . D7P1.solvePart1 $ input,
                             show . D7P2.solvePart2 $ input)
solveDay 8 input = return $ (show . D8P1.solvePart1 $ splitText input,
                             show . D8P2.solvePart2 $ splitText input)
solveDay 9 input = return $ (show . D9P1.solvePart1 $ input,
                             show . D9P2.solvePart2 $ input)
solveDay 10 input = do
  let p1 = show . D10P1.solvePart1 $ input
      p2 = "n/a"

  D10P2.solvePart2 input

  return (p1, p2)
solveDay 11 input = return $ (show . D11P1.solvePart1 $ input,
                              show . D11P2.solvePart2 $ input)
solveDay 12 input = return $ (show . D12P1.solvePart1 $ input,
                              show . D12P2.solvePart2 $ input)
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
