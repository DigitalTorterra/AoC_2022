module Aoc.Day.Five.Common where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()


-- Data types
data Instruction = Instruction Int Int Int deriving (Show)

data Stack a = Stack [a] deriving (Show)

empty :: Stack a
empty = Stack []

fromList :: [a] -> Stack a
fromList xs = Stack xs

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pushList :: [a] -> Stack a -> Stack a
pushList xs (Stack ys) = Stack ((reverse xs) ++ ys)

pushListStraight :: [a] -> Stack a -> Stack a
pushListStraight xs (Stack ys) = Stack (xs ++ ys)

pop :: Stack a -> (a, Stack a)
pop (Stack xs) = (head xs, Stack (tail xs))

popN :: Int -> Stack a -> ([a], Stack a)
popN n (Stack xs) = (take n xs, Stack (drop n xs))

replace :: Int -> a -> [a] -> [a]
replace idx x xs = map (\(i, val) -> if (i == idx) then x else val) $ zip [0..] xs


-- Parsing
parseInstruction :: Text -> Maybe Instruction
parseInstruction in_line = if valid
                              then Just $ Instruction (nums !! 0) (nums !! 1) (nums !! 2)
                            else Nothing
  where instruction_regex = "move ([0-9]+) from ([0-9]+) to ([0-9]+)" :: Text
        (l, c, r, subs) = (in_line =~ instruction_regex :: (Text, Text, Text, [Text]))
        valid = (T.null l) && (not $ T.null c) && (T.null r)
        nums = map (read . T.unpack) subs

getIndices :: Char -> String -> [Int]
getIndices c in_str = (map fst) . (filter (\(_, b) -> b == c)) $ zip [0..] in_str

parseStackLevel :: Text -> [(Int, Char)]
parseStackLevel in_line = zip stack_nums chars
  where in_str = T.unpack in_line
        indices = getIndices '[' in_str
        stack_nums = map (`div` 4) indices
        chars = map (\x -> in_str !! (x + 1)) indices


parseInput :: Text -> ([Stack Char], [Instruction])
parseInput in_text = (stacks, instructions)
  where in_lines = T.lines in_text
        instructions = mapMaybe parseInstruction in_lines
        stack_lines = filter (T.isInfixOf "[") in_lines
        num_stacks = maximum $ map (length . T.words) stack_lines
        stack_lines_parsed = concat $ map parseStackLevel stack_lines
        stacks = [fromList . (map snd) . (filter (\(a, _) -> a == idx)) $ stack_lines_parsed
                  | idx <- [0..num_stacks-1]]


        

-- Helpers
-- textToInt :: Text -> Int
-- textToInt = read . T.unpack

-- data Range t = Range t t deriving (Show, Eq)

-- rangeFromText :: Text -> Range Int
-- rangeFromText in_text = Range (textToInt first_part) (textToInt last_part)
--   where dash_split = T.splitOn "-" in_text
--         first_part = head dash_split
--         last_part = last dash_split


-- parseInput :: [Text] -> [(Range Int, Range Int)]
-- parseInput in_text = map f in_text
--   where f line = let comma_split = T.splitOn "," line
--                      first_part = head comma_split
--                      last_part = last comma_split
--                   in (rangeFromText first_part, rangeFromText last_part)
