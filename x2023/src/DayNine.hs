module DayNine (dayNine) where

import Data.Functor
import Solution
import Text.Parsec
import Text.Parsec.String (Parser)

parseInt :: Parser Int
parseInt = option id (char '-' $> negate) <*> (read <$> many1 digit)

parseHistory :: Parser [Int]
parseHistory = sepBy1 parseInt (string " ")

parseDeltas :: Parser [[Int]]
parseDeltas = reduce' <$> parseHistory
  where
    reduce' l =
      l
        : let l' = zipWith (-) (tail l) l
           in if all (0 ==) l' then [] else reduce' l'

parseReport :: ([Int] -> Int) -> Parser [[Int]]
parseReport f = many1 (map f <$> (parseDeltas <* spaces))

dayNine :: Solution Int
dayNine = Solution {partOne = sum . concat <$> parseReport last, partTwo = sum . map (foldr (-) 0) <$> parseReport head}
