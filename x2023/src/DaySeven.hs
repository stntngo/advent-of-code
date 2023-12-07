module DaySeven (daySeven) where

import Data.Functor
import Data.List (sort)
import Data.Map (Map, delete, empty, foldr, insertWith, lookup)
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

parseCard :: Int -> Parser Int
parseCard j =
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
      char 'J' $> j,
      char 'Q' $> 12,
      char 'K' $> 13,
      char 'A' $> 14
    ]

parseHand :: Parser (HandType, [Int])
parseHand = do
  cards <- many1 (parseCard 11)
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

parseJokerHand :: Parser (HandType, [Int])
parseJokerHand = do
  cards <- many1 (parseCard 1)
  let counts = Prelude.foldr (\c m -> insertWith (+) c 1 m) (empty :: Map Int Int) cards
  let jokers = Data.Map.lookup 1 counts
  let hand = Data.Map.foldr upgradeHand HighCard $ delete 1 counts
  return (upgradeJokers hand jokers, cards)
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

    upgradeJokers :: HandType -> Maybe Int -> HandType
    upgradeJokers t Nothing = t
    upgradeJokers t (Just n) =
      case t of
        HighCard | n == 1 -> OnePair
        HighCard | n == 2 -> ThreeOfAKind
        HighCard | n == 3 -> FourOfAKind
        HighCard | n >= 4 -> FiveOfAKind
        OnePair | n == 1 -> ThreeOfAKind
        OnePair | n == 2 -> FourOfAKind
        OnePair | n >= 3 -> FiveOfAKind
        TwoPair | n == 1 -> FullHouse
        TwoPair | n == 2 -> FourOfAKind
        TwoPair | n >= 3 -> FiveOfAKind
        ThreeOfAKind | n == 1 -> FourOfAKind
        ThreeOfAKind | n >= 2 -> FiveOfAKind
        FullHouse | n == 1 -> FourOfAKind
        FullHouse | n >= 2 -> FiveOfAKind
        FourOfAKind | n >= 1 -> FiveOfAKind
        _ -> t

parseBid :: Parser Int
parseBid = read <$> many1 digit

parsePlayer :: Parser ((HandType, [Int]), Int)
parsePlayer = do
  hand <- parseHand
  _ <- spaces
  bid <- parseBid
  return (hand, bid)

parseJokerPlayer :: Parser ((HandType, [Int]), Int)
parseJokerPlayer = do
  hand <- parseJokerHand
  _ <- spaces
  bid <- parseBid
  return (hand, bid)

scoreHands :: Parser [Int]
scoreHands = do
  players <- sort <$> many1 (parsePlayer <* spaces)
  return $ zipWith score [1 ..] players
  where
    score rank (_, bid) = rank * bid

scoreJokerHands :: Parser [Int]
scoreJokerHands = do
  players <- sort <$> many1 (parseJokerPlayer <* spaces)
  return $ zipWith score [1 ..] players
  where
    score rank (_, bid) = rank * bid

daySeven = Solution {partOne = sum <$> scoreHands, partTwo = sum <$> scoreJokerHands}
