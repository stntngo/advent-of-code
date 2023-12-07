module DayFive (dayFive) where

import Data.Map (Map, fromList, lookupLE)
import Data.Maybe (isJust)
import Solution
import Text.Parsec
import Text.Parsec.String (Parser)

chunksOf :: Int -> [a] -> [[a]]
chunksOf i ls = map (take i) (build (splitter ls))
  where
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n = l `c` splitter (drop i l) c n
    build g = g (:) []

data Mapping = Mapping {destStart :: Int, sourceStart :: Int, rangeLen :: Int} deriving (Show)

data Almanac = Almanac
  { seeds :: [Int],
    seedToSoil :: Map Int Mapping,
    soilToFertilizer :: Map Int Mapping,
    fertilizerToWater :: Map Int Mapping,
    waterToLight :: Map Int Mapping,
    lightToTemperature :: Map Int Mapping,
    temperatureToHumidity :: Map Int Mapping,
    humidityToLocation :: Map Int Mapping
  }
  deriving (Show)

parseDigit :: Parser Int
parseDigit = read <$> many1 digit

parseSeeds :: Parser [Int]
parseSeeds = string "seeds: " *> many1 (parseDigit <* spaces)

parseMapping :: Parser Mapping
parseMapping = do
  dest <- parseDigit <* spaces
  source <- parseDigit <* spaces
  range <- parseDigit <* spaces
  return Mapping {destStart = dest, sourceStart = source, rangeLen = range}

namedMapping :: (Mapping -> Int) -> String -> Parser (Map Int Mapping)
namedMapping k s = fromList . map (\m -> (k m, m)) <$> (string (s ++ " map:") *> spaces *> many1 parseMapping)

parseAlmanac :: (Mapping -> Int) -> Parser Almanac
parseAlmanac k = do
  let namedMapping' = namedMapping k
  seeds' <- parseSeeds <* spaces
  seedToSoil' <- namedMapping' "seed-to-soil"
  soilToFertilizer' <- namedMapping' "soil-to-fertilizer"
  fertilizerToWater' <- namedMapping' "fertilizer-to-water"
  waterToLight' <- namedMapping' "water-to-light"
  lightToTemperature' <- namedMapping' "light-to-temperature"
  temperatureToHumidity' <- namedMapping' "temperature-to-humidity"
  humidityToLocation' <- namedMapping' "humidity-to-location"
  () <- eof
  return
    Almanac
      { seeds = seeds',
        seedToSoil = seedToSoil',
        soilToFertilizer = soilToFertilizer',
        fertilizerToWater = fertilizerToWater',
        waterToLight = waterToLight',
        lightToTemperature = lightToTemperature',
        temperatureToHumidity = temperatureToHumidity',
        humidityToLocation = humidityToLocation'
      }

-- NOTE: (niels) This whole thing could be simplified by having
-- just a mapping list that I work through and then foldl through
-- that list.
findDestination :: Almanac -> Int -> Int
findDestination almanac =
  translate (humidityToLocation almanac)
    . translate (temperatureToHumidity almanac)
    . translate (lightToTemperature almanac)
    . translate (waterToLight almanac)
    . translate (fertilizerToWater almanac)
    . translate (soilToFertilizer almanac)
    . translate (seedToSoil almanac)
  where
    translate mapping k =
      case lookupLE k mapping of
        Nothing -> k
        Just (_, Mapping {destStart = dest, sourceStart = source, rangeLen = range}) -> if k < source + range then dest + (k - source) else k

parseLocations :: Parser [Int]
parseLocations = do
  almanac <- parseAlmanac sourceStart
  let locations = map (findDestination almanac) (seeds almanac)
  return locations

findSeed :: Almanac -> Int -> Int
findSeed almanac =
  translate (seedToSoil almanac)
    . translate (soilToFertilizer almanac)
    . translate (fertilizerToWater almanac)
    . translate (waterToLight almanac)
    . translate (lightToTemperature almanac)
    . translate (temperatureToHumidity almanac)
    . translate (humidityToLocation almanac)
  where
    translate mapping k =
      case lookupLE k mapping of
        Nothing -> k
        Just (_, Mapping {destStart = dest, sourceStart = source, rangeLen = range}) -> if k < dest + range then source + (k - dest) else k

parseDestinations :: Parser [Int]
parseDestinations = do
  almanac <- parseAlmanac destStart
  let matchRealSeeds = translate $ fromList $ map (\x -> (head x, x)) $ chunksOf 2 (seeds almanac)
  return (filter (isJust . matchRealSeeds . findSeed almanac) [0 ..])
  where
    translate seeds' k = do
      pair <- lookupLE k seeds'
      let (_, p) = pair
      let (x, y) = unpack p
      if (k - x) < y then return pair else fail ""

    unpack [x, y] = (x, y)
    unpack _ = error "pair must be exactly two elements"

dayFive :: Solution Int
dayFive = Solution {partOne = minimum <$> parseLocations, partTwo = head <$> parseDestinations}
