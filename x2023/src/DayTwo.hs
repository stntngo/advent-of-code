module DayTwo (dayTwo) where

import Control.Applicative ((<**>))
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

parsePull :: Parser Pull
parsePull =
  ignoreWS
    ( ignoreWS parseInt
        <**> choice
          [ string "red" $> Red,
            string "blue" $> Blue,
            string "green" $> Green
          ]
    )

parsePulls :: Parser [Pull]
parsePulls = ignoreWS (sepBy1 parsePull (char ','))

parseGame :: Parser Game
parseGame = do no <- parseGameNo; rounds <- parseRounds; return (Game (no, rounds))
  where
    parseGameNo = string "Game " *> parseInt <* char ':'
    parseRounds = ignoreWS (sepBy1 parsePulls (char ';'))

parseGames :: Parser [Game]
parseGames = many1 (parseGame <* spaces)

filterGames :: (Int, Int, Int) -> Parser [Game]
filterGames rgb = filter (validGame rgb) <$> parseGames

scoreValidGames :: (Int, Int, Int) -> Parser [Int]
scoreValidGames rgb = map gameId <$> filterGames rgb

validGame :: (Int, Int, Int) -> Game -> Bool
validGame (r, g, b) (Game (_, pulls)) = all validPulls pulls
  where
    validPull (Red x) = x <= r
    validPull (Green x) = x <= g
    validPull (Blue x) = x <= b
    validPulls = all validPull

gamePowers :: Parser [Int]
gamePowers =
  map power <$> parseGames
  where
    power (Game (_, pulls)) =
      let (r, g, b) = foldl update (0, 0, 0) (concat pulls)
       in r * g * b
      where
        update (r, g, b) (Red x) = (max r x, g, b)
        update (r, g, b) (Green x) = (r, max x g, b)
        update (r, g, b) (Blue x) = (r, g, max x b)

dayTwo :: Solution Int
dayTwo = Solution {partOne = sum <$> scoreValidGames (12, 13, 14), partTwo = sum <$> gamePowers}
