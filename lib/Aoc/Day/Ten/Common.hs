module Aoc.Day.Ten.Common where

import Data.Text (Text)
import qualified Data.Text as T
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Char as C
import Control.Applicative ((<|>))


-- Custom Data Types
data Instruction = Noop | Addx Int deriving (Show, Eq)

-- Parsing
addx :: ReadP Instruction
addx = do
  _ <- P.string "addx "
  num <- P.many1 $ P.satisfy (\c -> (C.isDigit c) || (c == '-'))
  _ <- P.char '\n'
  return $ Addx (read num)

noop :: ReadP Instruction
noop = do
  _ <- P.string "noop\n"
  return Noop

instruction :: ReadP Instruction
instruction = addx <|> noop

instructions :: ReadP [Instruction]
instructions = P.many1 instruction

parseInput :: Text -> [Instruction]
parseInput in_text = fst . last $ out_parsed
  where in_str = T.unpack in_text
        out_parsed = P.readP_to_S instructions in_str

-- Helpers
padInstrs :: [Instruction] -> [Instruction]
padInstrs instrs = concat $ map f instrs
  where f Noop = [Noop]
        f (Addx x) = [Noop, Addx x]
