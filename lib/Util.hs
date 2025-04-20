module Util where

-- import Control.DeepSeq (NFData)
import Control.Parallel.Strategies
import Data.List (foldl')
import Data.Text qualified as T
import Data.Text.Read qualified as TR

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

parseNumUnsafe :: (Integral a) => T.Text -> a
parseNumUnsafe s = case TR.decimal $ T.strip s of
  Right (x, _) -> x
  _ -> error "Invalid number string in input"

-- Parallel foldl' with a combining function for partial results
parFoldl' :: (NFData b) => (b -> a -> b) -> (b -> b -> b) -> b -> [a] -> b
parFoldl' f g z xs = foldl' g z $ parMap rdeepseq (foldl' f z) chunks
  where
    -- Split list into chunks based on number of cores
    numCores = 24 -- Adjust based on system or use GHC.Conc.numCapabilities
    chunkSize = max 1 (length xs `div` numCores)
    chunks = chunkList chunkSize xs

-- Helper to split list into chunks
chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs
  | length xs <= n = [xs]
  | otherwise = take n xs : chunkList n (drop n xs)
