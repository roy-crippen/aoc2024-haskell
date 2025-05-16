{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day12 where

import Data.FileEmbed (embedFile)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector.Unboxed qualified as VU
import Grid qualified as G
import Text.Heredoc (here)
import Util (Solution (..))

parse :: T.Text -> G.Grid Char
parse text =
  let ls = T.lines text
      numRows = length ls
      numCols = case ls of
        [] -> error "parse: input must have at least one row"
        (l : _) -> T.length l
      dataVec = VU.fromList $ concatMap T.unpack ls
   in G.Grid
        { G.vals = dataVec,
          G.rows = numRows,
          G.cols = numCols
        }

groupByChar :: G.Grid Char -> M.Map Char [G.Pos]
groupByChar g = M.fromListWith (++) $ mapMaybe (\p -> (,[p]) <$> G.get g p) ps
  where
    ps = [G.Pos (x, y) | x <- [0 .. g.rows - 1], y <- [0 .. g.cols - 1]]

distinctAreas :: G.Grid Char -> M.Map Char [G.Pos] -> [[G.Pos]]
distinctAreas g charMap = concat $ M.elems $ M.map findAreas charMap
  where
    findAreas :: [G.Pos] -> [[G.Pos]]
    findAreas ps = go (Set.fromList ps) []
      where
        go unvisited areas
          | Set.null unvisited = areas
          | otherwise =
              let start = Set.findMin unvisited
                  (area, newUnvisited) = dfs start unvisited
               in go newUnvisited (area : areas)

        dfs :: G.Pos -> Set.Set G.Pos -> ([G.Pos], Set.Set G.Pos)
        dfs start unvisited = dfs' [start] (Set.delete start unvisited) []
          where
            dfs' [] unvis area = (area, unvis)
            dfs' (p : rest) unvis area =
              let neighbors = [n | n <- G.neighbors4 p, G.isInside g n, Set.member n unvis, G.get g n == G.get g p]
                  newUnvis = foldr Set.delete unvis neighbors
               in dfs' (neighbors ++ rest) newUnvis (p : area)

perimeter :: G.Grid Char -> [G.Pos] -> Int
perimeter g ps = sum [4 - length (likeNeighbors p) | p <- ps]
  where
    areaSet = Set.fromList ps
    likeNeighbors p' = [n | n <- G.neighbors4 p', G.isInside g n, Set.member n areaSet]

solutionDay12 :: Solution
solutionDay12 =
  Solution
    { day = 12,
      input = textInput,
      solvePart1 = part1,
      solvePart2 = part2,
      expectedPart1 = 1361494,
      expectedPart2 = 1361494
    }

textInput :: T.Text
textInput = TE.decodeUtf8 $(embedFile "data/day_12.txt")

part1 :: T.Text -> Int
part1 s = sum $ map (\area -> perimeter g area * length area) areas
  where
    g = parse s
    m = groupByChar g
    areas = distinctAreas g m

part2 :: T.Text -> Int
part2 s = sum $ map (\area -> perimeter g area * length area) areas
  where
    g = parse s
    m = groupByChar g
    areas = distinctAreas g m

exampleText :: T.Text
exampleText =
  T.pack
    [here|RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE|]
