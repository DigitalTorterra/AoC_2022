module Aoc.Day.Eleven.PartOne (solvePart1) where

import Data.Text (Text)
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Text.IO as TIO

import Aoc.Day.Eleven.Common


-- Get the worry after examining
monkeyWorry :: Monkey -> Int -> Int
monkeyWorry mokey = (`div` 3) . (performOperation (monkeyOperation mokey))

-- Perform divisibility test
monkeyPeformTest :: Monkey -> Int -> Bool
monkeyPeformTest mokey x = (x `mod` (monkeyTest mokey)) == 0

-- Figure out who the monkey targets
monkeyTarget :: Monkey -> Int -> Int
monkeyTarget mokey x = f $ monkeyTargets mokey
  where f = if (monkeyPeformTest mokey x) then fst else snd

monkeyVerdict :: Monkey -> Int -> (Int, Int)
monkeyVerdict mokey x = (new_target, new_worry)
  where new_worry = monkeyWorry mokey x
        new_target = monkeyTarget mokey new_worry

-- Helper functions
getMonkeyBusiness :: State [Monkey] Int
getMonkeyBusiness = do
  monkeys <- get
  return . product . (take 2) . reverse . L.sort . (map monkeyNumInspected) $ monkeys


giveMonkeyItem :: (Int, Int) -> State [Monkey] ()
giveMonkeyItem (target, worry) = do
  monkeys <- get

  let my_monkey = monkeys !! target
      my_stack = monkeyStack my_monkey
      new_stack = addObject worry my_stack
      new_monkey = my_monkey { monkeyStack = new_stack }
      new_state = setMonkey new_monkey monkeys

  put new_state


monkeyPass :: Int -> State [Monkey] ()
monkeyPass monkey_idx = do
  -- Parse the state
  monkeys <- get

  let curr_monkey = getMonkey monkey_idx monkeys
      (Holding curr_items) = monkeyStack curr_monkey
      item_destins = map (monkeyVerdict curr_monkey) curr_items
      curr_num_inspected = monkeyNumInspected curr_monkey
      new_monkey = curr_monkey { monkeyStack = Holding []
                               , monkeyNumInspected = curr_num_inspected + (length curr_items)
                               }

  -- Replace the current monkey
  put $ setMonkey new_monkey monkeys

  -- Give all the monkeys their items
  mapM_ giveMonkeyItem item_destins


monkeyFun :: Int -> State [Monkey] Int
monkeyFun num_rounds = do
  monkey_idxs <- gets (map monkeyId)

  -- Iterate over each round
  _ <- forM [1..num_rounds] $ \_ -> do
    forM monkey_idxs $ \monkey_idx -> do
      monkeyPass monkey_idx

  -- Finally, get the monkey business
  getMonkeyBusiness


solvePart1 :: Text -> Int
solvePart1 in_text = evalState (monkeyFun 20) monkeys
  where monkeys = parseInput in_text


-- main :: IO ()
-- main = do
--   let in_path = "./puzzle-inputs/day11_test.txt"
--   in_text <- TIO.readFile in_path

--   let monkeys = parseInput in_text
--       s4 = evalState (monkeyFun 20) monkeys
--       -- s1 = execState (monkeyPass 0) monkeys
--       -- s2 = execState (monkeyPass 1) s1
--       -- s3 = execState (monkeyPass 2) s2
--       -- s4 = execState (monkeyPass 3) s3
--       -- s1 = execState (monkeyFun 1) monkeys

--   putStrLn . show $ s4
