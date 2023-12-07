module DaySix (daySix) where

import Solution
import Text.Parsec
import Text.Parsec.String (Parser)

data Race = Race {time :: Int, distance :: Int} deriving (Show)

parseSingleDigit :: Parser Int
parseSingleDigit = read <$> many1 (spaces *> digit <* spaces)

parseDigit :: Parser Int
parseDigit = read <$> many1 digit

parseTimes :: Parser Int -> Parser [Int]
parseTimes d = string "Time:" *> many1 (spaces *> d <* spaces)

parseDistances :: Parser Int -> Parser [Int]
parseDistances d = string "Distance:" *> many1 (spaces *> d <* spaces)

parseRaces :: Parser Int -> Parser [Race]
parseRaces dig = do
  times <- parseTimes dig
  zipWith (\t d -> Race {time = t, distance = d}) times <$> parseDistances dig

-- NOTE: (niels) I'm here to do PROGRAMMERING not MATH
doRace :: Int -> [Int]
doRace t = map (\h -> h * (t - h)) [0 .. t]

scoreRace :: Race -> Int
scoreRace Race {time = t, distance = d} = length <$> filter (> d) $ doRace t

sol :: Parser Int -> Parser Int
sol d = product . map scoreRace <$> parseRaces d

daySix :: Solution Int
daySix = Solution {partOne = sol parseDigit, partTwo = sol parseSingleDigit}
