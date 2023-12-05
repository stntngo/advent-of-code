module DayFive (dayFive) where

import Data.Map (Map, fromList, lookupLE)
import Solution
import Text.Parsec
import Text.Parsec.String (Parser)

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

namedMapping :: String -> Parser (Map Int Mapping)
namedMapping s = fromList . map (\m -> (sourceStart m, m)) <$> (string (s ++ " map:") *> spaces *> many1 parseMapping)

parseAlmanac :: Parser Almanac
parseAlmanac = do
  seeds' <- parseSeeds <* spaces
  seedToSoil' <- namedMapping "seed-to-soil"
  soilToFertilizer' <- namedMapping "soil-to-fertilizer"
  fertilizerToWater' <- namedMapping "fertilizer-to-water"
  waterToLight' <- namedMapping "water-to-light"
  lightToTemperature' <- namedMapping "light-to-temperature"
  temperatureToHumidity' <- namedMapping "temperature-to-humidity"
  humidityToLocation' <- namedMapping "humidity-to-location"
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
findLocation :: Almanac -> Int -> Int
findLocation almanac =
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
  almanac <- parseAlmanac
  let locations = map (findLocation almanac) (seeds almanac)
  return locations

dayFive :: Solution Int
dayFive = Solution {partOne = minimum <$> parseLocations, partTwo = return (-1)}
