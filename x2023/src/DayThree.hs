{-# LANGUAGE TupleSections #-}

module DayThree (dayThree) where

import Data.Map (Map, alter, empty, filter, lookup)
import Data.Maybe (mapMaybe)
import Data.Set (Set, fromList, member)
import Solution
import Text.Parsec
import Text.Parsec.Pos ()
import Text.Parsec.String (Parser)

loc :: Parser (Int, Int)
loc = do
  pos <- getPosition
  let line = sourceLine pos
  let col = sourceColumn pos
  return (line, col)

data PartNumber = PartNumber {partStart :: (Int, Int), partEnd :: (Int, Int), partNum :: Int} deriving (Show)

data Symbol = Symbol {symStart :: (Int, Int), symEnd :: (Int, Int), symbol :: Char} deriving (Show)

data Entry
  = Part PartNumber
  | Sym Symbol
  deriving (Show)

parsePartNumber :: Parser PartNumber
parsePartNumber = do
  start' <- loc
  number' <- read <$> many1 digit
  (line, col) <- loc
  return (PartNumber {partStart = start', partEnd = (line, col - 1), partNum = number'})

parseSymbol :: Parser Symbol
parseSymbol = do
  start' <- loc
  symbol' <- noneOf ".\n"
  (line, col) <- loc
  return (Symbol {symStart = start', symEnd = (line, col - 1), symbol = symbol'})

parseEntry :: Parser Entry
parseEntry = Part <$> parsePartNumber <|> Sym <$> parseSymbol

dots :: Parser [String]
dots = many (string ".")

parseEntries :: Parser [Entry]
parseEntries = many ((dots *> parseEntry <* dots) <* spaces)

splitEntries :: [Entry] -> ([PartNumber], [Symbol])
splitEntries = foldr (flip update) ([], [])
  where
    update (parts, syms) (Part x) = (parts ++ [x], syms)
    update (parts, syms) (Sym x) = (parts, syms ++ [x])

findGears :: Parser [[PartNumber]]
findGears = do
  (parts, syms) <- splitEntries <$> parseEntries
  let symLocs = map symStart syms
  let neighbors = foldr (flip populate) empty parts
  let filtered = Data.Map.filter (\x -> length x == 2) neighbors
  let gears = mapMaybe (`Data.Map.lookup` filtered) symLocs
  return gears
  where
    populate :: Map (Int, Int) [PartNumber] -> PartNumber -> Map (Int, Int) [PartNumber]
    populate m p =
      let conns = connections (partStart p) (partEnd p)
       in let update' = flip (alter (update p))
           in foldr (flip update') m conns

    update x Nothing = Just [x]
    update x (Just xs) = Just (xs ++ [x])

combineGears :: Parser [Int]
combineGears = map combine <$> findGears
  where
    combine [PartNumber {partNum = x}, PartNumber {partNum = y}] = x * y
    combine _ = error "gears must be exactly two parts"

filterParts :: Parser [PartNumber]
filterParts = do
  (parts, symList) <- splitEntries <$> parseEntries
  let symbols = fromList (map symStart symList)
  let filtered = Prelude.filter (touchesSymbol symbols) parts
  return filtered

connections :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
connections (line, left) (_, right) =
  [(line, left - 1), (line, right + 1)]
    ++ map (line - 1,) [left - 1 .. right + 1]
    ++ map (line + 1,) [left - 1 .. right + 1]

touchesSymbol :: Set (Int, Int) -> PartNumber -> Bool
touchesSymbol symbols (PartNumber {partStart = start, partEnd = end}) =
  let conns = connections start end
   in any (`member` symbols) conns

dayThree :: Solution Int
dayThree = Solution {partOne = sum . map partNum <$> filterParts, partTwo = sum <$> combineGears}
