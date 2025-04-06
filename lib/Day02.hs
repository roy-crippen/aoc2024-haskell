{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day02 where

import Control.Parallel.Strategies (parMap, rseq)
import Data.FileEmbed (embedFile)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Data.Vector qualified as V
import Text.Heredoc (here)
import Util (Solution (..))

parse :: T.Text -> V.Vector (V.Vector Int)
parse inStr = V.fromList $ map parseLine $ filter (not . T.null) $ T.lines $ T.strip inStr
  where
    parseLine line = V.fromList $ map parseNum $ T.words line
    parseNum s = case TR.decimal s of
      Right (n, rest) | T.null rest -> n
      _ -> error "Invalid number string in input"

isSafe :: V.Vector Int -> Bool
isSafe vec
  | V.length vec == 0 = False
  | V.length vec == 1 = True
  | otherwise = go (V.head vec) (V.tail vec)
  where
    go prev rest
      | V.null rest = True
      | otherwise =
          let curr = V.head rest
              diff = abs (curr - prev)
           in diff < 4
                && if curr > prev -- 0 to 3
                  then checkAscending prev rest
                  else checkDescending prev rest

    -- recursive ascending check
    checkAscending prev rest
      | V.null rest = True
      | otherwise =
          let curr = V.head rest
              diff = abs (curr - prev)
           in diff < 4 && curr > prev && checkAscending curr (V.tail rest)

    -- recursive scending check
    checkDescending prev rest
      | V.null rest = True
      | otherwise =
          let curr = V.head rest
              diff = abs (prev - curr)
           in diff < 4 && curr < prev && checkDescending curr (V.tail rest)

isSafePart2 :: V.Vector Int -> Bool
isSafePart2 vec
  | isSafe vec = True
  | otherwise = V.foldl' (\acc i -> acc || isSafe (dropAt vec i)) False (V.enumFromN 0 (V.length vec))
  where
    dropAt v i = V.take i v V.++ V.drop (i + 1) v

solutionDay02 :: Solution
solutionDay02 =
  Solution
    { day = 2,
      input = textInput,
      solvePart1 = part1,
      solvePart2 = part2,
      expectedPart1 = 390,
      expectedPart2 = 439
    }

textInput :: T.Text
textInput = TE.decodeUtf8 $(embedFile "data/day_02.txt")

part1 :: T.Text -> Int
part1 s = sum $ parMap rseq (\row -> if isSafe row then 1 else 0) (V.toList (parse s))

part2 :: T.Text -> Int
part2 s = sum $ parMap rseq (\row -> if isSafePart2 row then 1 else 0) (V.toList (parse s))

exampleText :: T.Text
exampleText =
  T.pack
    [here|
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
|]