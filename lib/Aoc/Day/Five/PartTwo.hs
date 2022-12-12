module Aoc.Day.Five.PartTwo (solvePart2) where

import Data.Text (Text)
import Control.Monad
import Control.Monad.State

import Aoc.Day.Five.Common

runInstruction :: Instruction -> [Stack Char] -> [Stack Char]
runInstruction (Instruction n f t) stacks = out
  where from_stack = stacks !! (f - 1)
        to_stack = stacks !! (t - 1)
        (vals, from_stack') = popN n from_stack
        to_stack' = pushListStraight vals to_stack
        out = (replace (f-1) from_stack') . (replace (t-1) to_stack') $ stacks

runInstructions :: [Instruction] -> State [Stack Char] ()
runInstructions instrs = forM_ instrs $ \instr -> do
  modify $ runInstruction instr


solvePart2 :: Text -> String
solvePart2 in_text = final_str
  where (stacks, instrs) = parseInput in_text
        final_vals = execState (runInstructions instrs) stacks
        final_str = map (fst . pop) final_vals

