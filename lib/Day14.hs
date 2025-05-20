{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day14 where

-- import Debug.Trace (trace)

import Control.Parallel.Strategies (parMap, rseq)
import Data.FileEmbed (embedFile)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Text.Heredoc (here)
import Util (Solution (..), chunkList, parseNumUnsafe)

type Idx = Int

type Delta = Int

type Length = Int

type Count = Idx

parse :: T.Text -> Int -> Int -> [(Idx, Delta, Length)]
parse s rows cols = concatMap (\t -> parseValues t rows cols) (T.lines s)

parseValues :: T.Text -> Int -> Int -> [(Idx, Delta, Length)]
parseValues s rows cols = [(x, vx, cols'), (y, vy, rows')]
  where
    rows' = rows - 1
    cols' = cols - 1
    ls = T.splitOn (T.pack " ") s
    (x, y) = parseValue $ head ls
    (vx, vy) = parseValue $ ls !! 1

parseValue :: T.Text -> (Int, Int)
parseValue s = (v1, v2)
  where
    (v1, v2) = case T.splitOn (T.pack ",") s of
      [s1, s2] -> (parseNumUnsafe $ T.drop 1 $ T.dropWhile (/= '=') s1, parseNumUnsafe s2)
      _ -> error "Invalid input"

move :: (Idx, Delta, Length) -> Idx
move (idx, delta, len) = case divs of
  0 -> remainder
  _ -> if remainder == 0 then len else remainder - 1
  where
    (divs, remainder) = (idx + delta) `divMod` len

moves :: (Idx, Delta, Length) -> Count -> Idx
moves (idx, delta, len) count = case count of
  0 -> idx
  _ -> moves (move (idx, delta, len), delta, len) (count - 1)

doMoves :: (Idx, Delta, Length) -> Count -> Idx
doMoves (idx, delta, len) count =
  if delta > 0
    then moves (idx, delta, len) count
    else len - moves (len - idx, abs delta, len) count

makePairs :: [(Idx, Delta, Length)] -> Count -> [(Int, Int)]
makePairs vs count = filter (\(x, y) -> x /= midX && y /= midY) xyPairs
  where
    xyPairs = map (\ls -> (head ls, ls !! 1)) $ chunkList 2 (parMap rseq (`doMoves` count) vs)
    (_, _, cols) = head vs
    (_, _, rows) = vs !! 1
    midX = cols `div` 2
    midY = rows `div` 2

score :: [(Int, Int)] -> Int -> Int -> Int
score xs rows cols = q1 * q2 * q3 * q4
  where
    -- msg = "count (q1,q2,q3,q4) = (" ++ show q1 ++ "," ++ show q2 ++ "," ++ show q3 ++ "," ++ show q4 ++ ")"
    midX = cols `div` 2
    midY = rows `div` 2
    (q1, q2, q3, q4) = foldl getScore (0, 0, 0, 0) xs

    getScore :: (Int, Int, Int, Int) -> (Int, Int) -> (Int, Int, Int, Int)
    getScore (q1', q2', q3', q4') (x, y) = case (lowX, lowY) of
      (True, True) -> (q1' + 1, q2', q3', q4')
      (True, False) -> (q1', q2', q3' + 1, q4')
      (False, True) -> (q1', q2' + 1, q3', q4')
      (False, False) -> (q1', q2', q3', q4' + 1)
      where
        lowX = x <= midX - 1
        lowY = y <= midY - 1

solve :: [(Idx, Delta, Length)] -> Length -> Length -> Count -> Int
solve xs rows cols count = score (makePairs xs count) rows cols

solutionDay14 :: Solution
solutionDay14 =
  Solution
    { day = 14,
      input = textInput,
      solvePart1 = part1,
      solvePart2 = part2,
      expectedPart1 = 220971520,
      expectedPart2 = 42
    }

textInput :: T.Text
textInput = TE.decodeUtf8 $(embedFile "data/day_14.txt")

part1 :: T.Text -> Int
part1 s = solve (parse s rows cols) rows cols 100
  where
    -- rows = 7
    -- cols = 11

    rows = 103
    cols = 101

part2 :: T.Text -> Int
part2 _s = 42

exampleText :: T.Text
exampleText =
  T.pack
    [here|p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3|]