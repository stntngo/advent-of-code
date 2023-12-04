module DayFour (dayFour) where

import Data.Map (Map, empty, findWithDefault, foldr, fromAscList, insert, lookup)
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set, fromList, member)
import Solution
import Text.Parsec
import Text.Parsec.Pos ()
import Text.Parsec.String (Parser)

data Card = Card {idx :: Int, winners :: Set Int, numbers :: [Int], matches :: Int} deriving (Show)

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseCardNo :: Parser Int
parseCardNo = (string "Card" <* spaces) *> parseInt <* string ":"

parseNumbers :: Parser [Int]
parseNumbers = many1 (spaces *> parseInt <* spaces)

parseWinners :: Parser (Set Int)
parseWinners = fromList <$> parseNumbers

parseCard :: Parser Card
parseCard = do
  cardNo <- parseCardNo
  cardWinners <- parseWinners
  _ <- spaces *> string "|" <* spaces
  cardNumbers <- parseNumbers
  let matches' = length $ filter (`member` cardWinners) cardNumbers
  return
    Card
      { idx = cardNo,
        winners = cardWinners,
        numbers = cardNumbers,
        matches = matches'
      }

parseCards :: Parser [Card]
parseCards = many1 (parseCard <* spaces)

parseCardMap :: [Card] -> Map Int Card
parseCardMap = fromAscList . zip [1 ..] -- NOTE: (niels) I initially wrote [0 ..] here and I was so confused why my stupid idea wasn't working...

scoreCard :: Card -> Int
scoreCard Card {matches = m} | m > 0 = 2 ^ (m - 1)
scoreCard _ = 0

countCards :: Map Int Card -> Card -> Map Int Int -> Map Int Int
countCards cards (Card {idx = cardIdx, matches = matches'}) scores =
  let extras = mapMaybe (`Data.Map.lookup` cards) [cardIdx + 1 .. cardIdx + matches']
   in let found = sum $ map find' extras
       in Data.Map.insert cardIdx (found + 1) scores
  where
    find' card =
      findWithDefault
        (fromJust $ Data.Map.lookup (idx card) $ countCards cards card scores)
        (idx card)
        scores

accumlateCards :: Parser Int
accumlateCards = do
  cards <- parseCards
  let m = parseCardMap cards
  let scores = Prelude.foldr (countCards m) empty cards
  let score = Data.Map.foldr (+) 0 scores
  return score

dayFour :: Solution Int
dayFour =
  Solution
    { partOne = sum . map scoreCard <$> parseCards,
      partTwo = accumlateCards
    }
