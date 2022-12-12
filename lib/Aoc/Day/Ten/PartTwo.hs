module Aoc.Day.Ten.PartTwo (solvePart2) where

import Data.Text (Text)
import Data.List.Split (splitPlaces)
import qualified Data.List as L
-- import Control.Monad.State

import Aoc.Day.Ten.Common
import Aoc.Utils


data CRTState = CRTState { clock :: Int
                         , register :: Int
                         , pixel :: Int
                         , lit :: Bool
                         } deriving (Show)


simulateInstrs :: [Instruction] -> [CRTState]
simulateInstrs instrs = tail $ scanl f (CRTState {clock=0, register=1, pixel=(-1), lit=False}) instrs
  where f :: CRTState -> Instruction -> CRTState
        f crt instr = let
          new_clock = (clock crt) + 1
          old_register = register crt
          new_register = case instr of
            Noop -> old_register
            Addx x -> old_register + x
          old_pixel = pixel crt
          new_pixel = (old_pixel + 1) `mod` 40
          new_lit = (diff old_register new_pixel) < 2
          in CRTState {clock=new_clock, register=new_register, pixel=new_pixel, lit=new_lit}


-- solvePart2 :: Text -> Int
solvePart2 :: Text -> IO ()
solvePart2 in_text = do
  putStrLn "\n--- Day 10, Part 2 ---"

  -- Parse input
  let states = simulateInstrs . padInstrs . parseInput $ in_text
      lits = map (\s -> if (lit s) then '#' else '.') states
      split_lit = concat . (L.intersperse "\n") $ splitPlaces (repeat (40 :: Int)) lits

  putStrLn split_lit

  putStrLn ""
-- solvePart2 in_text = sum $ map signal relevant
--   where instrs = parseInput in_text
--         simulated = simulateInstrs instrs
--         relevant = filter (\cpu -> (clock cpu) `mod` 40 == 20) simulated
