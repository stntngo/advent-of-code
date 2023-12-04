module Parser (parse) where

import System.Exit
import System.IO
import Text.Parsec.String (Parser, parseFromFile)

parse :: Parser a -> String -> IO a
parse p f = parseFromFile p f >>= either report return
  where
    report err = do
      hPutStrLn stderr $ "Error: " ++ show err
      exitFailure
