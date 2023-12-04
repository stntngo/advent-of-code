{-# LANGUAGE FlexibleContexts #-}

module DayTwo (dayTwo) where

import Data.Functor
import Solution
import Text.Parsec
import Text.Parsec.Char ()
import Text.Parsec.String (Parser)

data Pull = Red Int | Green Int | Blue Int deriving (Show)

newtype Game = Game (Int, [[Pull]]) deriving (Show)

gameId :: Game -> Int
gameId (Game (idx, _)) = idx

parseInt :: Parser Int
parseInt = read <$> many1 digit

ignoreWS :: Parser a -> Parser a
ignoreWS p = spaces *> p <* spaces

bind :: Parser a -> Parser (a -> b) -> Parser b
bind p q = do x <- p; f <- q; return (f x)

parsePull :: Parser Pull
parsePull =
  ignoreWS
    ( bind
        (ignoreWS parseInt)
        ( choice
            [ string "red" $> Red,
              string "blue" $> Blue,
              string "green" $> Green
            ]
        )
    )

parsePulls :: Parser [Pull]
parsePulls = ignoreWS (sepBy1 parsePull (string ","))

parseGame :: Parser Game
parseGame = do no <- parseGameNo; rounds <- parseRounds; return (Game (no, rounds))
  where
    parseGameNo = string "Game " *> parseInt <* string ":"
    parseRounds = ignoreWS (sepBy1 parsePulls (string ";"))

parseGames :: Parser [Game]
parseGames = many1 (parseGame <* spaces)

filterGames :: (Int, Int, Int) -> Parser [Game]
filterGames rgb = filter (validGame rgb) <$> parseGames

scoreValidGames :: (Int, Int, Int) -> Parser Int
scoreValidGames rgb = sum . map gameId <$> filterGames rgb

validGame :: (Int, Int, Int) -> Game -> Bool
validGame (r, g, b) (Game (_, pulls)) = all validPulls pulls
  where
    validPull (Red x) = x <= r
    validPull (Green x) = x <= g
    validPull (Blue x) = x <= b
    validPulls = all validPull

gamePowers :: Parser [Int]
gamePowers = map power <$> parseGames

-- foo :: Game -> (Int, (Int, Int, Int))
power :: Game -> Int
power (Game (_, pulls)) =
  let (r, g, b) = foldl update (0, 0, 0) (concat pulls)
   in r * g * b

update :: (Int, Int, Int) -> Pull -> (Int, Int, Int)
update (r, g, b) (Red x) = (max r x, g, b)
update (r, g, b) (Green x) = (r, max x g, b)
update (r, g, b) (Blue x) = (r, g, max x b)

dayTwo :: Solution Int
dayTwo = Solution {partOne = scoreValidGames (12, 13, 14), partTwo = sum <$> gamePowers}
