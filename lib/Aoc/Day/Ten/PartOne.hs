module Aoc.Day.Ten.PartOne (solvePart1) where

import Data.Text (Text)
-- import Control.Monad.State

import Aoc.Day.Ten.Common



data CPUState = CPUState { clock :: Int
                         , register :: Int
                         , signal :: Int
                         } deriving (Show)


simulateInstrs :: [Instruction] -> [CPUState]
simulateInstrs instrs = scanl f (CPUState {clock=0, register=1, signal=0}) padded
  where padded = padInstrs instrs
        f :: CPUState -> Instruction -> CPUState
        f cpu instr = let
          new_clock = (clock cpu) + 1
          new_register = case instr of
            Noop -> register cpu
            Addx x -> (register cpu) + x
          new_signal = new_clock * (register cpu)
          in CPUState {clock=new_clock, register=new_register, signal=new_signal}

solvePart1 :: Text -> Int
solvePart1 in_text = sum $ map signal relevant
  where instrs = parseInput in_text
        simulated = simulateInstrs instrs
        relevant = filter (\cpu -> (clock cpu) `mod` 40 == 20) simulated
