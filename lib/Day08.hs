{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day08 where

import Data.DList qualified as DList
import Data.FileEmbed (embedFile)
import Data.HashSet qualified as HashSet
import Data.List (foldl')
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Text.Heredoc (here)
import Util (Solution (..), kCombosWithoutReps)

type Pos = (Int, Int)

data Board = Board {rows :: Int, cols :: Int, ants :: [[Pos]]} deriving (Show)

(++.) :: DList.DList a -> DList.DList a -> DList.DList a
(++.) = DList.append

parse :: T.Text -> Board
parse text = board
  where
    ls = T.lines $ T.strip text
    rows' = length ls
    cols' = T.length $ head ls
    antsMap = foldr addAnt M.empty $ concat $ zipWith (\r row -> zipWith (\c ch -> (r, c, ch)) [0 ..] (T.unpack row)) [0 ..] ls
    addAnt (r, c, ch) m
      | ch == '.' = m
      | otherwise = M.insertWith (++) ch [(r, c)] m
    ants' = M.elems antsMap
    board = Board {rows = rows', cols = cols', ants = ants'}

getCombos :: [Pos] -> [(Pos, Pos)]
getCombos vs = map toTuple $ kCombosWithoutReps 2 vs
  where
    toTuple ps = case ps of
      [p1, p2] -> (p1, p2)
      _ -> error "invalid shape making positions tuple"

antiPair :: Pos -> Pos -> Int -> Int -> Maybe Pos
antiPair (r1, c1) (r2, c2) rows cols = maybePos
  where
    r3 = 2 * r1 - r2
    c3 = 2 * c1 - c2
    maybePos =
      if r3 >= 0 && r3 < rows && c3 >= 0 && c3 < cols
        then Just (r3, c3)
        else Nothing

antisPart1 :: [(Pos, Pos)] -> Int -> Int -> [Pos]
antisPart1 combos rows cols = concatMap process combos
  where
    process (p1, p2) = catMaybes [antiPair p1 p2 rows cols, antiPair p2 p1 rows cols]

manyAntis :: Pos -> Pos -> Int -> Int -> [Pos]
manyAntis p1' p2' rows cols = go p1' p2' []
  where
    go :: Pos -> Pos -> [Pos] -> [Pos]
    go p1 p2 res = case antiPair p1 p2 rows cols of
      Just p3 -> go p2 p3 res' where res' = go p3 p1 (p3 : res)
      _ -> res

antisPart2 :: [(Pos, Pos)] -> Int -> Int -> [Pos]
antisPart2 combos rows cols = DList.toList $ foldr process DList.empty combos
  where
    process (p1, p2) acc = list1 ++. list2 ++. acc
      where
        list1 = DList.fromList $ manyAntis p1 p2 rows cols
        list2 = DList.fromList $ manyAntis p2 p1 rows cols

solutionDay08 :: Solution
solutionDay08 =
  Solution
    { day = 08,
      input = textInput,
      solvePart1 = part1,
      solvePart2 = part2,
      expectedPart1 = 396,
      expectedPart2 = 1200
    }

textInput :: T.Text
textInput = TE.decodeUtf8 $(embedFile "data/day_08.txt")

part1 :: T.Text -> Int
part1 s = length $ HashSet.fromList antis
  where
    b = parse s
    f acc ant = acc ++ antisPart1 (getCombos ant) (rows b) (cols b)
    antis = foldl' f [] (ants b)

part2 :: T.Text -> Int
part2 s = HashSet.size hs
  where
    b = parse s
    f acc ant = foldr HashSet.insert acc (ant ++ antisPart2 (getCombos ant) (rows b) (cols b))
    hs = foldl' f HashSet.empty (ants b)

exampleText :: T.Text
exampleText =
  T.pack
    [here|............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............|]