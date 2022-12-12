module Aoc.Day.Seven.Common where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as C
import qualified Data.Maybe as M
import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP
import Control.Monad.State as S

-- Foundational Data Types
data Inode = Directory String [Inode] | File String Int deriving (Show, Eq)
data Movement = Root | Up | Into String deriving (Show)
data Instruction = Listing [Inode] | Change Movement deriving (Show)

data Path = Path [String] deriving (Show, Eq)

isFile :: Inode -> Bool
isFile (File _ _) = True
isFile (Directory _ _) = False

isDirectory :: Inode -> Bool
isDirectory = not . isFile

parseDir :: Inode -> Maybe (String, [Inode])
parseDir (Directory s l) = Just (s, l)
parseDir _ = Nothing

inodeSize :: Inode -> Int
inodeSize (File _ sz) = sz
inodeSize (Directory _ is) = sum $ map inodeSize is

-- Parsing
-- Repeated patterns
text :: ReadP String
text = many1 $ satisfy C.isLetter

fname :: ReadP String
fname = many1 $ satisfy (\c -> C.isLetter c || c == '.')

number :: ReadP Int
number = do
  nums <- many1 $ satisfy C.isDigit
  return $ read nums

discardChar :: Char -> ReadP ()
discardChar c = do
  _ <- satisfy (==c)
  return ()

discardString :: String -> ReadP ()
discardString s = do
  _ <- string s
  return ()

newLine :: ReadP ()
newLine = discardChar '\n'

space :: ReadP ()
space = discardChar ' '

-- cd
up :: ReadP Movement
up = string ".." >>= \_ -> return Up

root :: ReadP Movement
root = string "/" >>= \_ -> return Root

into :: ReadP Movement
into = do
  dir <- text
  return $ Into dir

movement :: ReadP Movement
movement = up <|> root <|> into

change :: ReadP Instruction
change = do
  discardString "$ cd "
  mvmt <- movement
  newLine
  return $ Change mvmt

-- ls
directory :: ReadP Inode
directory = do
  discardString "dir "
  dir <- text
  newLine
  return $ Directory dir []

file :: ReadP Inode
file = do
  nums <- number
  space
  name <- fname
  newLine
  return $ File name nums

inode :: ReadP Inode
inode = directory <|> file

list :: ReadP Instruction
list = do
  discardString "$ ls\n"
  matches <- many inode
  return $ Listing matches

instruction :: ReadP Instruction
instruction = change <|> list

instructions :: ReadP [Instruction]
instructions = many instruction

parseInstructions :: Text -> [Instruction]
parseInstructions in_text = fst . last $ readP_to_S instructions (T.unpack in_text)


-- Tree Construction
modifyPath :: Movement -> Path -> Path
modifyPath in_mvmt (Path in_path) = case in_mvmt of
  Root -> Path []
  Up -> Path $ tail in_path
  Into dest -> Path $ dest:in_path

execInstruction :: Instruction -> State Path [(Path, Inode)]
execInstruction (Listing inodes) = do
  curr_path <- S.get
  return $ zip (repeat curr_path) inodes
execInstruction (Change mvmt) = do
  S.modify (modifyPath mvmt)
  return []

unrollInstructions :: [Instruction] -> State Path [(Path, Inode)]
unrollInstructions instrs = do
  res <- mapM execInstruction instrs
  return $ concat res

constructTree :: [(Path, Inode)] -> Inode
constructTree xs = Directory "/" (f root_path)
  where root_path = Path []
        f :: Path -> [Inode]
        f curr_path = all_files ++ all_full_dirs
          where matching = filter ((== curr_path) . fst) xs
                all_inodes = map snd matching
                all_files = filter isFile all_inodes
                all_dirs = filter isDirectory all_inodes
                all_dirnames = map (fst . M.fromJust . parseDir) all_dirs
                all_full_dirs = [Directory dirname (f (modifyPath (Into dirname) curr_path))
                                | dirname <- all_dirnames]

-- Overall function
parseInput :: Text -> Inode
parseInput in_text = constructTree unrolled_instrs
  where parsed_instrs = parseInstructions in_text
        unrolled_instrs = S.evalState (unrollInstructions parsed_instrs) (Path [])
