module Main (main) where

import Control.Monad ()
import Data.List ()
import DayOne (dayOne)
import DayThree (dayThree)
import DayTwo (dayTwo)
import Parser (parse)
import Solution (Solution, norm, partOne, partTwo)
import System.Console.GetOpt
import System.Environment

solutions :: [Solution String]
solutions = [norm dayOne, norm dayTwo, norm dayThree]

main :: IO ()
main = do
  argv <- getArgs
  days <- opts argv
  let solutions' = map (\x -> (solutions !! (x - 1), x)) days
  mapM_ run solutions'
  where
    opts argv =
      case getOpt RequireOrder [] argv of
        ([], days, []) -> return (map (\d -> read d :: Int) days)
        (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header []))
      where
        header = "Usage: x2023 [OPTION...] files..."
    run (sol, day) = do
      p1 <- parse (partOne sol) ("input/day" ++ show day)
      p2 <- parse (partTwo sol) ("input/day" ++ show day)
      putStrLn ("Day " ++ show day ++ " Part One: " ++ p1)
      putStrLn ("Day " ++ show day ++ " Part Two: " ++ p2)
