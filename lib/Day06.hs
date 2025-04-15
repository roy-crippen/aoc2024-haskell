{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day06 where

import Data.FileEmbed (embedFile)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector.Unboxed qualified as VU
import Grid qualified as G
import Text.Heredoc (here)
import Util (Solution (..))

data State = State {pos :: G.Pos, dir :: G.Dir, rows :: Int, cols :: Int} deriving (Show)

data Status = Done | OutOfBounds | Running deriving (Show, Eq)

parse :: T.Text -> (G.Grid Char, State)
parse text = (g, st)
  where
    ls = T.lines text
    numRows = length ls
    numCols = case ls of
      [] -> error "parse: input must have at least one row"
      (l : _) -> T.length l
    dataVec = VU.fromList $ concatMap T.unpack ls
    g = G.Grid {G.vals = dataVec, G.rows = numRows, G.cols = numCols}
    startPos = head $ G.findPositions g (== '^')
    st = State {pos = startPos, dir = G.N, rows = numRows, cols = numCols}

traverseGrid :: G.Grid Char -> State -> [G.Pos] -> [G.Pos]
traverseGrid g st xs = go st xs Running
  where
    go state ps status
      | status == OutOfBounds = ps
      | status == Done = case dir state of
          G.N -> traverseGrid g (state {dir = G.E}) ps
          G.E -> traverseGrid g (state {dir = G.S}) ps
          G.S -> traverseGrid g (state {dir = G.W}) ps
          G.W -> traverseGrid g (state {dir = G.N}) ps
          _ -> error "illegal direction encountered"
      | otherwise = go state' ps' status'
      where
        (state', ps', status') = slideToNext g state ps

slideToNext :: G.Grid Char -> State -> [G.Pos] -> (State, [G.Pos], Status)
slideToNext g = go
  where
    go state acc = case step state of
      (state', newPositions, Running) -> go state' (newPositions ++ acc)
      (state', _, status') -> (state', acc, status')
    step state =
      let newPos = G.move (pos state) (dir state)
          isInside = G.isInside g newPos
          isGuard = isInside && (G.get g newPos == Just '#')
       in case (isInside, isGuard) of
            (True, True) -> (state, [], Done)
            (False, _) -> (state, [], OutOfBounds)
            _ -> (state {pos = newPos}, [newPos], Running)

solutionDay06 :: Solution
solutionDay06 =
  Solution
    { day = 06,
      input = textInput,
      solvePart1 = part1,
      solvePart2 = part2,
      expectedPart1 = 5329,
      expectedPart2 = 2162
    }

textInput :: T.Text
textInput = TE.decodeUtf8 $(embedFile "data/day_06.txt")

part1 :: T.Text -> Int
part1 s =
  let (g, st) = parse s
      xs = traverseGrid g st [pos st]
   in length $ Set.fromList xs

part2 :: T.Text -> Int
part2 _s = 42

exampleText :: T.Text
exampleText =
  T.pack
    [here|....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
|]