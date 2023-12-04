{-# LANGUAGE FlexibleContexts #-}

module DayOne (dayOne) where

import Data.Char (digitToInt)
import Data.Functor
import Data.Maybe (catMaybes)
import Solution
import Text.Parsec (ParsecT, Stream, anyChar, choice, lookAhead, many, many1, spaces, string, try, (<|>))
import Text.Parsec.Char (digit, letter)
import Text.Parsec.String (Parser)

calibration :: [String] -> Int
calibration s =
  let start = (head . head) s
   in let end = (last . last) s
       in read [start, end]

parseLine :: Parser Int
parseLine = calibration <$> (many letter *> many1 (many1 digit <* many letter))

parseCalibration :: Parser Int
parseCalibration = sum <$> many1 (parseLine <* spaces)

tryString :: (Stream s m Char) => String -> ParsecT s u m String
tryString = try . string

parseNumber :: Parser Int
parseNumber =
  lookAhead
    ( choice
        [ tryString "one" $> 1,
          tryString "two" $> 2,
          tryString "three" $> 3,
          tryString "four" $> 4,
          tryString "five" $> 5,
          tryString "six" $> 6,
          tryString "seven" $> 7,
          tryString "eight" $> 8,
          tryString "nine" $> 9
        ]
    )
    <* anyChar

parseElement :: Parser (Maybe Int)
parseElement =
  (Just <$> (parseNumber <|> (digitToInt <$> digit)))
    <|> letter $> Nothing

calibration' :: [Int] -> Int
calibration' s =
  let start = head s
   in let end = last s
       in start * 10 + end

parseLine' :: Parser Int
parseLine' = calibration' . catMaybes <$> many1 parseElement

parseCalibration' :: Parser Int
parseCalibration' = sum <$> many1 (parseLine' <* spaces)

dayOne :: Solution Int
dayOne = Solution {partOne = parseCalibration, partTwo = parseCalibration'}
