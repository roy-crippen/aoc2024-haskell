{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day14 where

-- import Control.Parallel.Strategies (parMap, rseq)
import Data.FileEmbed (embedFile)
import Data.List (group, sort, sortBy)
import Data.Ord (comparing)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Debug.Trace (trace)
import Text.Heredoc (here)
import Util (Solution (..), parseNumUnsafe)

type Idx = Int

type Delta = Int

type Length = Int

type Count = Idx

parse :: T.Text -> ([(Idx, Delta)], [(Idx, Delta)])
parse s = unzip . sortYX $ map parseValues (T.lines s)

parseValues :: T.Text -> ((Idx, Delta), (Idx, Delta))
parseValues s = ((x, vx), (y, vy))
  where
    ls = T.splitOn (T.pack " ") s
    (x, y) = parseValue $ head ls
    (vx, vy) = parseValue $ ls !! 1

parseValue :: T.Text -> (Int, Int)
parseValue s = (v1, v2)
  where
    (v1, v2) = case T.splitOn (T.pack ",") s of
      [s1, s2] -> (parseNumUnsafe $ T.drop 1 $ T.dropWhile (/= '=') s1, parseNumUnsafe s2)
      _ -> error "Invalid input"

sortYX :: [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
sortYX = sortBy (\(x1, y1) (x2, y2) -> comparing fst y1 y2 <> comparing fst x1 x2)

process :: [(Idx, Delta)] -> Length -> Count -> [Idx]
process pairs len count = map (\(x, vx) -> (x + vx * count) `mod` len) pairs

scorePart1 :: [(Int, Int)] -> Int -> Int -> Int
scorePart1 xs midX midY = q1 * q2 * q3 * q4
  where
    -- msg = "count (q1,q2,q3,q4) = (" ++ show q1 ++ "," ++ show q2 ++ "," ++ show q3 ++ "," ++ show q4 ++ ")"
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

solutionDay14 :: Solution
solutionDay14 =
  Solution
    { day = 14,
      input = textInput,
      solvePart1 = part1,
      solvePart2 = part2,
      expectedPart1 = 220971520,
      expectedPart2 = 6355
    }

textInput :: T.Text
textInput = TE.decodeUtf8 $(embedFile "data/day_14.txt")

part1 :: T.Text -> Int
part1 s = scorePart1 filteredPts midX midY
  where
    rows = 103
    cols = 101
    midX = cols `div` 2
    midY = rows `div` 2
    (xs, ys) = parse s
    xyPairs = zip (process xs cols 100) (process ys rows 100)
    filteredPts = filter (\(x, y) -> x /= midX && y /= midY) xyPairs

part2 :: T.Text -> Int
part2 s = go 6000 -- needs fixing
-- faster sort container -> Data.Vector.Unboxed Bool
  where
    go :: Count -> Count
    go count =
      if length xGroups > 1 && length yGroups > 10
        then count -- trace (show count ++ ", " ++ show (sort xIndexes))
        else go (count + 1)
      where
        xIndexes = process xs cols count
        xGroups = filter (\vs -> length vs > 30) $ group (sort xIndexes)
        yIndexes = process ys rows count
        yGroups = filter (\vs -> length vs > 10) $ group (sort yIndexes)

    rows = 103
    cols = 101
    (xs, ys) = parse s

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