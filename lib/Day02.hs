{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day02 where

import Data.FileEmbed (embedFile)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Data.Vector qualified as V
import Text.Heredoc (here)
import Util (Solution (..))

parse :: T.Text -> V.Vector (V.Vector Int)
parse inStr = V.fromList $ map parseLine $ T.lines $ T.strip inStr
  where
    parseLine line = V.fromList $ map parseNum $ T.splitOn (T.pack " ") line
    parseNum s = case TR.decimal s of
      Right (n, rest) | T.null rest -> n
      _ -> error "Invalid number string in input"

isSafe :: V.Vector Int -> Bool
isSafe vec
  | V.length vec <= 1 = True
  | otherwise =
      let firstDiff = V.head (V.tail vec) - V.head vec
       in if firstDiff > 0
            then checkAscending (V.head vec) (V.tail vec)
            else checkDescending (V.head vec) (V.tail vec)
  where
    checkAscending prev rest
      | V.null rest = True
      | otherwise =
          let curr = V.head rest
              diff = curr - prev
           in diff >= 1 && diff <= 3 && checkAscending curr (V.tail rest)
    checkDescending prev rest
      | V.null rest = True
      | otherwise =
          let curr = V.head rest
              diff = prev - curr
           in diff >= 1 && diff <= 3 && checkDescending curr (V.tail rest)

isSafePart2 :: V.Vector Int -> Bool
isSafePart2 vec
  | isSafe vec = True
  | otherwise = V.any isSafe $ V.imap (\i _ -> V.ifilter (\j _ -> i /= j) vec) vec

solutionDay02 :: Solution
solutionDay02 =
  Solution
    { day = 02,
      input = textInput,
      solvePart1 = part1,
      solvePart2 = part2,
      expectedPart1 = 390,
      expectedPart2 = 439
    }

textInput :: T.Text
textInput = TE.decodeUtf8 $(embedFile "data/day_02.txt")

part1 :: T.Text -> Int
part1 s = V.foldl' (\acc row -> if isSafe row then acc + 1 else acc) 0 (parse s)

part2 :: T.Text -> Int
part2 s = V.foldl' (\acc row -> if isSafePart2 row then acc + 1 else acc) 0 (parse s)

exampleText :: T.Text
exampleText =
  T.pack
    [here|
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9|]