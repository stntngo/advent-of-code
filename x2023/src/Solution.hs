module Solution (Solution (..), norm) where

import Text.Parsec.String (Parser)

data (Show a) => Solution a = Solution
  { partOne :: Parser a,
    partTwo :: Parser a
  }

norm :: (Show a) => Solution a -> Solution String
norm p = Solution {partOne = show <$> partOne p, partTwo = show <$> partTwo p}
