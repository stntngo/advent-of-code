module DayEight (dayEight) where

import Data.Functor
import Data.Map (Map, fromList, keys, lookup)
import Data.Maybe (fromJust)
import Solution
import Text.Parsec
import Text.Parsec.String (Parser)

data Direction = L | R deriving (Show, Eq)

parseDir :: Parser Direction
parseDir = (char 'L' $> L) <|> char 'R' $> R

parseDirs :: Parser [Direction]
parseDirs = many1 parseDir <* spaces

parseLabel :: Parser String
parseLabel = count 3 anyChar

parseTuple :: Parser (String, String)
parseTuple = do
  _ <- char '('
  l <- parseLabel
  _ <- char ','
  _ <- space
  r <- parseLabel
  _ <- char ')'
  return (l, r)

parseNode :: Parser (String, (String, String))
parseNode = do
  label' <- parseLabel
  _ <- space
  _ <- char '='
  _ <- space
  dest <- parseTuple
  _ <- spaces
  return (label', dest)

parseMap :: Parser ([Direction], Map String (String, String))
parseMap = do
  dirs <- parseDirs
  connections <- fromList <$> many1 parseNode
  return (dirs, connections)

navigate :: Map String (String, String) -> String -> [Direction] -> [String]
navigate m n ds
  | n !! 2 == 'Z' = []
  | otherwise =
      let n' = (if head ds == L then fst else snd) $ fromJust $ Data.Map.lookup n m
       in n : navigate m n' (tail ds)

parseDistance :: Parser Int
parseDistance = do
  (dirs, m) <- parseMap
  return (length $ navigate m "AAA" (cycle dirs))

parseGhostDistance :: Parser [Int]
parseGhostDistance = do
  (dirs, m) <- parseMap
  let starts = filter (\k -> k !! 2 == 'A') (keys m)
  return (map (\n -> length $ navigate m n (cycle dirs)) starts)

dayEight :: Solution Int
dayEight = Solution {partOne = parseDistance, partTwo = foldr lcm 1 <$> parseGhostDistance}
