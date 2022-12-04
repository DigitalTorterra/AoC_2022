module Aoc.Utils where

import Data.Text (Text)
import qualified Data.Text as T

splitText :: Text -> [Text]
splitText in_text = filter (not . T.null) (T.splitOn "\n" in_text)
