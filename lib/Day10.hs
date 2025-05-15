{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day10 where

import Data.Char (ord)
import Data.FileEmbed (embedFile)
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector.Unboxed qualified as VU
import Grid qualified as G
import Text.Heredoc (here)
import Util (Solution (..))

parse :: T.Text -> G.Grid Word
parse text = G.Grid {G.vals = dataVec, G.rows = numRows, G.cols = numCols}
  where
    ls = T.lines text
    numRows = length ls
    numCols = case ls of
      [] -> error "parse: input must have at least one row"
      (l : _) -> T.length l
    dataVec = VU.fromList $ map (\c -> (fromIntegral . ord) c - 48) $ concatMap T.unpack ls

scores :: G.Grid Word -> G.Pos -> [G.Pos]
scores = scores_loop
  where
    scores_loop g p = case G.get g p of
      Nothing -> []
      Just val
        | val == 9 -> [p]
        | otherwise ->
            let neighbors = G.apply4 g p G.get
                validNeighbors = catMaybes $ zipWith (\n m -> (n,) <$> m) [G.north p, G.west p, G.south p, G.east p] neighbors
             in concatMap (\(np, v) -> if v == val + 1 then scores_loop g np else []) validNeighbors

solutionDay10 :: Solution
solutionDay10 =
  Solution
    { day = 10,
      input = textInput,
      solvePart1 = part1,
      solvePart2 = part2,
      expectedPart1 = 667,
      expectedPart2 = 1344
    }

textInput :: T.Text
textInput = TE.decodeUtf8 $(embedFile "data/day_10.txt")

part1 :: T.Text -> Int
part1 s = List.foldl' (\acc pos -> acc + uniqueLength pos) 0 positions
  where
    g = parse s
    positions = G.findPositions g (== 0)
    uniqueLength pos = length $ HashSet.toList $ HashSet.fromList (scores g pos)

part2 :: T.Text -> Int
part2 s = List.foldl' (\acc pos -> acc + length (scores g pos)) 0 positions
  where
    g = parse s
    positions = G.findPositions g (== 0)

exampleText :: T.Text
exampleText =
  T.pack
    [here|89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
|]