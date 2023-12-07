module DaySeven (daySeven) where

import Data.Functor
import Data.List (sort)
import Data.Map (Map, empty, foldr, insertWith)
import Solution
import Text.Parsec
import Text.Parsec.String (Parser)

data HandType
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Show, Eq, Ord)

parseCard :: Parser Int
parseCard =
  choice
    [ char '2' $> 2,
      char '3' $> 3,
      char '4' $> 4,
      char '5' $> 5,
      char '6' $> 6,
      char '7' $> 7,
      char '8' $> 8,
      char '9' $> 9,
      char 'T' $> 10,
      char 'J' $> 11,
      char 'Q' $> 12,
      char 'K' $> 13,
      char 'A' $> 14
    ]

parseHand :: Parser (HandType, [Int])
parseHand = do
  cards <- many1 parseCard
  let hand = Data.Map.foldr upgradeHand HighCard $ Prelude.foldr (\c m -> insertWith (+) c 1 m) (empty :: Map Int Int) cards
  return (hand, cards)
  where
    upgradeHand :: Int -> HandType -> HandType
    upgradeHand c t =
      case (t, c) of
        (_, 5) -> FiveOfAKind
        (_, 4) -> FourOfAKind
        (OnePair, 3) -> FullHouse
        (_, 3) -> ThreeOfAKind
        (OnePair, 2) -> TwoPair
        (ThreeOfAKind, 2) -> FullHouse
        (_, 2) -> OnePair
        (_, _) -> t

parseBid :: Parser Int
parseBid = read <$> many1 digit

parsePlayer :: Parser ((HandType, [Int]), Int)
parsePlayer = do
  hand <- parseHand
  _ <- spaces
  bid <- parseBid
  return (hand, bid)

scoreHands :: Parser [Int]
scoreHands = do
  players <- sort <$> many1 (parsePlayer <* spaces)
  return $ zipWith score [1 ..] players
  where
    score rank (_, bid) = rank * bid

daySeven = Solution {partOne = sum <$> scoreHands, partTwo = sum <$> scoreHands}
