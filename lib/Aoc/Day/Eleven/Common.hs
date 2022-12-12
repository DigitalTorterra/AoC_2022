module Aoc.Day.Eleven.Common where

import Data.Text (Text)
import qualified Data.Text as T
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Char as C
import Control.Applicative
import qualified Data.List as L
import qualified Data.Maybe as M

-- Custom Data Types
data Holding a = Holding [a] deriving (Show)

data Operand = Old | Value Int deriving (Show, Eq)
data Operator = Plus | Minus | Times deriving (Show)

type Operation = (Operand, Operator, Operand)

data Monkey = Monkey { monkeyId :: Int
                     , monkeyStack :: Holding Int
                     , monkeyOperation :: Operation
                     , monkeyTest :: Int
                     , monkeyTargets :: (Int, Int)
                     , monkeyNumInspected :: Int
                     } deriving (Show)

-- Helper functions
performOperation :: Operation -> Int -> Int
performOperation (op1, operator, op2) start_val = op1_val `f` op2_val
  where op1_val = case op1 of Old -> start_val
                              Value x -> x
        op2_val = case op2 of Old -> start_val
                              Value x -> x
        f = case operator of Plus -> (+)
                             Minus -> (-)
                             Times -> (*)

numHolding :: Holding a -> Int
numHolding (Holding xs) = length xs

addObject :: a -> Holding a -> Holding a
addObject x (Holding xs) = Holding $ xs ++ [x]

-- modifyList :: Int -> (a -> a) -> [a] -> [a]
-- modifyList idx f xs = map f_gated $ zip [0..] xs
--   where f_gated = \(i, x) -> if i == idx then f x else x

getMonkey :: Int -> [Monkey] -> Monkey
getMonkey idx monkeys = M.fromJust $ L.find (\m -> idx == monkeyId m) monkeys

setMonkey :: Monkey -> [Monkey] -> [Monkey]
setMonkey monkey monkeys = map f monkeys
  where f m = if (monkeyId m == monkeyId monkey) then monkey else m




-- Parsing
dropString :: String -> ReadP ()
dropString in_str = P.string in_str >> return ()

dropNewline :: ReadP ()
dropNewline = P.char '\n' >> return ()

numberP :: ReadP Int
numberP = do
  num_str <- P.many1 $ P.satisfy C.isDigit
  return $ read num_str

numListP :: ReadP [Int]
numListP = P.sepBy1 numberP (dropString ", ")

monkeyIdP :: ReadP Int
monkeyIdP = do
  dropString "Monkey "
  num <- numberP
  dropString ":\n"
  return num

monkeyStackP :: ReadP (Holding Int)
monkeyStackP = do
  P.skipSpaces
  dropString "Starting items: "
  num_list <- numListP
  dropNewline

  return $ Holding num_list

monkeyOperandP :: ReadP Operand
monkeyOperandP =
  let old = do
        dropString "old"
        return Old
      val = do
        num <- numberP
        return $ Value num
  in old <|> val

monkeyOperatorP :: ReadP Operator
monkeyOperatorP = do
  let f :: Char -> Bool
      f c = c `elem` ['+', '-', '*']
  c <- P.satisfy f
  return $ case c of
    '+' -> Plus
    '-' -> Minus
    '*' -> Times
    _ -> Plus

monkeyOperationP :: ReadP Operation
monkeyOperationP = do
  P.skipSpaces
  dropString "Operation: new = "
  op1 <- monkeyOperandP
  P.skipSpaces
  oper <- monkeyOperatorP
  P.skipSpaces
  op2 <- monkeyOperandP
  dropNewline

  return (op1, oper, op2)

monkeyTestP :: ReadP Int
monkeyTestP = do
  P.skipSpaces
  dropString "Test: divisible by "
  num <- numberP
  dropNewline

  return num

monkeyTargetsP :: ReadP (Int, Int)
monkeyTargetsP = do
  P.skipSpaces
  dropString "If true: throw to monkey "
  num1 <- numberP
  dropNewline

  P.skipSpaces
  dropString "If false: throw to monkey "
  num2 <- numberP
  dropNewline

  return (num1, num2)

monkeyP :: ReadP Monkey
monkeyP = do
  monkey_id <- monkeyIdP
  monkey_stack <- monkeyStackP
  monkey_operation <- monkeyOperationP
  monkey_test <- monkeyTestP
  monkey_targets <- monkeyTargetsP

  return $ Monkey { monkeyId = monkey_id
                  , monkeyStack = monkey_stack
                  , monkeyOperation = monkey_operation
                  , monkeyTest = monkey_test
                  , monkeyTargets = monkey_targets
                  , monkeyNumInspected = 0
                  }

monkeysP :: ReadP [Monkey]
monkeysP = P.sepBy1 monkeyP (P.char '\n')

parseInput :: Text -> [Monkey]
parseInput in_text = fst . last $ parsed
  where in_str = T.unpack in_text
        parsed = P.readP_to_S monkeysP in_str

