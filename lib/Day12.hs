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
parse text = G.Grid {G.vals = dataVec, G.rows = numRows, G.cols = numCols}
  where
    ls = T.lines text
    numRows = length ls
    numCols = case ls of
      [] -> error "parse: input must have at least one row"
      (l : _) -> T.length l
    dataVec = VU.fromList $ concatMap T.unpack ls

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
        go :: Set.Set G.Pos -> [[G.Pos]] -> [[G.Pos]]
        go unvisited areas
          | Set.null unvisited = areas
          | otherwise = go newUnvisited (area : areas)
          where
            start = Set.findMin unvisited
            (area, newUnvisited) = dfs start unvisited

        dfs :: G.Pos -> Set.Set G.Pos -> ([G.Pos], Set.Set G.Pos)
        dfs start unvisited = dfs' [start] (Set.delete start unvisited) []
          where
            dfs' [] unvis area = (area, unvis)
            dfs' (p : rest) unvis area = dfs' (neighbors ++ rest) newUnvis (p : area)
              where
                neighbors = [n | n <- G.neighbors4 p, Set.member n unvis, G.isInside g n, G.get g n == ch]
                ch = G.get g p
                newUnvis = foldr Set.delete unvis neighbors

perimeter :: G.Grid Char -> [G.Pos] -> Int
perimeter g ps = sum [4 - length (likeNeighbors p) | p <- ps]
  where
    likeNeighbors p' = [n | n <- G.neighbors4 p', G.isInside g n && G.get g n == ch]
      where
        ch = G.get g p'

cornerCounts :: G.Grid Char -> [G.Pos] -> Int
cornerCounts g ps = sum $ map cornerCount ps
  where
    cornerCount p = count
      where
        (n, nw, w, sw, s, se, e, ne) = G.neighborValues8' g p
        ch = G.get g p
        count =
          (if s == ch && se /= ch && e == ch then 1 else 0)
            + (if s == ch && sw /= ch && w == ch then 1 else 0)
            + (if n == ch && ne /= ch && e == ch then 1 else 0)
            + (if n == ch && nw /= ch && w == ch then 1 else 0)
            + (if s /= ch && e /= ch then 1 else 0)
            + (if s /= ch && w /= ch then 1 else 0)
            + (if n /= ch && e /= ch then 1 else 0)
            + (if n /= ch && w /= ch then 1 else 0)

solutionDay12 :: Solution
solutionDay12 =
  Solution
    { day = 12,
      input = textInput,
      solvePart1 = part1,
      solvePart2 = part2,
      expectedPart1 = 1361494,
      expectedPart2 = 830516
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
part2 s = sum $ map (\area -> cornerCounts g area * length area) areas
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

e1 :: T.Text
e1 =
  T.pack
    [here|AAAA
BBCD
BBCC
EEEC|]

e2 :: T.Text
e2 =
  T.pack
    [here|EEEEE
EXXXX
EEEEE
EXXXX
EEEEE|]

e3 :: T.Text
e3 =
  T.pack
    [here|AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA|]
