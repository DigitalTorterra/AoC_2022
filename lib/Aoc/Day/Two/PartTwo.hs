module Aoc.Day.Two.PartTwo (solvePart2) where

-- Data type
data Hand = Rock | Paper | Scissors deriving (Show, Eq)
data Verdict = Win | Lose | Draw deriving (Show)

charToHand :: Char -> Hand
charToHand c = case c of
  'A' -> Rock
  'B' -> Paper
  'C' -> Scissors

charToVerdict :: Char -> Verdict
charToVerdict 'X' = Lose
charToVerdict 'Y' = Draw
charToVerdict 'Z' = Win

winningHand :: Hand -> Hand
winningHand Rock = Paper
winningHand Paper = Scissors
winningHand Scissors = Rock

losingHand :: Hand -> Hand
losingHand Rock = Scissors
losingHand Paper = Rock
losingHand Scissors = Paper

drawingHand :: Hand -> Hand
drawingHand = id

-- Scoring
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

-- Added complexity
getChoice :: Verdict -> Hand -> Hand
getChoice Lose = losingHand
getChoice Draw = drawingHand
getChoice Win = winningHand

scoreStrategy :: (Hand, Verdict) -> Int
scoreStrategy (h, v) = scoreMatch (h, getChoice v h)

solvePart2 :: [(Char, Char)] -> Int
solvePart2 throws = sum $ map (\(a, b) -> scoreStrategy (charToHand a, charToVerdict b)) throws
