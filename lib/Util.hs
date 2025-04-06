module Util where

import Data.Text qualified as T

data Solution = Solution
  { day :: Int,
    input :: T.Text,
    solvePart1 :: T.Text -> Int,
    solvePart2 :: T.Text -> Int,
    expectedPart1 :: Int,
    expectedPart2 :: Int
  }

instance Show Solution where
  show :: Solution -> String
  show s =
    "Solution {"
      ++ "day = "
      ++ show (day s)
      ++ ", input = "
      ++ show (input s)
      ++ ", part1 = \"run part1 function\""
      ++ ", part2 = \"run part2 function\""
      ++ ", expectedPart1 = "
      ++ show (expectedPart1 s)
      ++ ", expectedPart2 = "
      ++ show (expectedPart2 s)
      ++ "}"