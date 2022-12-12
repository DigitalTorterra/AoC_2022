module Aoc.Day.Nine.Common where

import Data.Text (Text)
import qualified Data.Text as T
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Char as C

import Aoc.Utils

-- Custom data types
data Direction = Left' | Right' | Up' | Down' deriving (Show, Eq)
data Movement = Movement Direction Int deriving (Show, Eq)

type Point = (Int, Int)


-- Parsing
direction :: ReadP Direction
direction = do
  in_char <- P.satisfy (`elem` ("RLUD" :: String))
  return $ case in_char of
    'R' -> Right'
    'L' -> Left'
    'U' -> Up'
    'D' -> Down'
    _ -> Down'

amount :: ReadP Int
amount = do
  in_num <- P.many1 $ P.satisfy C.isDigit
  return $ read in_num

movement :: ReadP Movement
movement = do
  dir <- direction
  P.skipSpaces
  amt <- amount
  _ <- P.char '\n'
  return $ Movement dir amt

movements :: ReadP [Movement]
movements = P.many1 movement

parseLines :: Text -> [Movement]
parseLines in_lines = fst . last $ P.readP_to_S movements (T.unpack in_lines)

-- Movement
goDirection :: Int -> Int -> Int
goDirection target curr
  | target > curr = 1
  | target < curr = -1
  | otherwise = 0

moveHead :: Point -> Direction -> Point
moveHead (i, j) Left' = (i-1, j)
moveHead (i, j) Right' = (i+1, j)
moveHead (i, j) Up' = (i, j+1)
moveHead (i, j) Down' = (i, j-1)

moveTail :: Point -> Point -> Point
moveTail (ih, jh) (it, jt)
  | (ih == it) && ((diff jh jt) > 1) = (it, jt + (goDirection jh jt))
  | (jh == jt) && ((diff ih it) > 1) = (it + (goDirection ih it), jt)
  | ((diff ih it) < 2) && ((diff jh jt) < 2) = (it, jt)
  | otherwise = (it + (goDirection ih it), jt + (goDirection jh jt))

unrollInstructions :: [Movement] -> [Direction]
unrollInstructions = concat . (map (\(Movement dir amt) -> take amt $ repeat dir))
