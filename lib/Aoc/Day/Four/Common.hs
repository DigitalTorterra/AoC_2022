module Aoc.Day.Four.Common where

import Data.Text (Text)
import qualified Data.Text as T

-- Helpers
textToInt :: Text -> Int
textToInt = read . T.unpack

data Range t = Range t t deriving (Show, Eq)

rangeFromText :: Text -> Range Int
rangeFromText in_text = Range (textToInt first_part) (textToInt last_part)
  where dash_split = T.splitOn "-" in_text
        first_part = head dash_split
        last_part = last dash_split


parseInput :: [Text] -> [(Range Int, Range Int)]
parseInput in_text = map f in_text
  where f line = let comma_split = T.splitOn "," line
                     first_part = head comma_split
                     last_part = last comma_split
                  in (rangeFromText first_part, rangeFromText last_part)
