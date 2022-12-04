module Aoc.Day.Two.PartOne (solvePart1) where


-- Data type
data Hand = Rock | Paper | Scissors deriving (Show, Eq)
data Verdict = Win | Lose | Draw deriving (Show)

charToHand :: Char -> Hand
charToHand c = case c of
  'A' -> Rock
  'B' -> Paper
  'C' -> Scissors
  'X' -> Rock
  'Y' -> Paper
  'Z' -> Scissors

rightResult :: (Hand, Hand) -> Verdict
rightResult (x, y)
  | x == y = Draw
  | ((x == Rock) && (y == Paper)) ||
    ((x == Paper) && (y == Scissors)) ||
    ((x == Scissors) && (y == Rock)) = Win
  | otherwise = Lose

scoreHand :: Hand -> Int
scoreHand Rock = 1
scoreHand Paper = 2
scoreHand Scissors = 3

scoreVerdict :: Verdict -> Int
scoreVerdict Lose = 0
scoreVerdict Draw = 3
scoreVerdict Win = 6

scoreMatch :: (Hand, Hand) -> Int
scoreMatch (x, y) = (scoreHand y) + (scoreVerdict $ rightResult (x, y))

solvePart1 :: [(Char, Char)] -> Int
solvePart1 throws = sum $ map (\(a, b) -> scoreMatch (charToHand a, charToHand b)) throws
