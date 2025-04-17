{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day06 where

import Control.Monad (when)
import Control.Monad.ST (runST)
import Control.Parallel.Strategies (parMap, rseq)
import Data.FileEmbed (embedFile)
import Data.HashSet qualified as HashSet
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
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

traverseGridPart1 :: G.Grid Char -> State -> [G.Pos] -> [G.Pos]
traverseGridPart1 g st xs = go st xs Running
  where
    go state ps status
      | status == OutOfBounds = ps
      | status == Done = case dir state of
          G.N -> traverseGridPart1 g (state {dir = G.E}) ps
          G.E -> traverseGridPart1 g (state {dir = G.S}) ps
          G.S -> traverseGridPart1 g (state {dir = G.W}) ps
          G.W -> traverseGridPart1 g (state {dir = G.N}) ps
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

toNext :: G.Grid Char -> State -> (State, Status)
toNext g state =
  let newPos = G.move (pos state) (dir state)
      G.Pos (r, c) = newPos
      isInside = r >= 0 && r < G.rows g && c >= 0 && c < G.cols g
      idx = G.posToIdx newPos (G.cols g)
      isGuard = isInside && VU.unsafeIndex (G.vals g) idx == '#'
      result = case (isInside, isGuard) of
        (True, True) -> (state, Done)
        (False, _) -> (state, OutOfBounds)
        (True, False) -> (state {pos = newPos}, Running)
   in result
{-# INLINE toNext #-}

nextDir :: G.Dir -> G.Dir
nextDir G.N = G.E
nextDir G.E = G.S
nextDir G.S = G.W
nextDir G.W = G.N
nextDir _ = error "illegal direction"

-- Map (Pos, Dir) to index
dirToInt :: G.Dir -> Int
dirToInt G.N = 0
dirToInt G.E = 1
dirToInt G.S = 2
dirToInt G.W = 3
dirToInt _ = error "invalid direction"
{-# INLINE dirToInt #-}

posDirToIndex :: Int -> G.Pos -> G.Dir -> Int
posDirToIndex cols (G.Pos (r, c)) dir = (r * cols + c) * 4 + dirToInt dir
{-# INLINE posDirToIndex #-}

traverseGridPart2 :: G.Grid Char -> State -> Bool
traverseGridPart2 g st = runST $ do
  let numRows = G.rows g
      numCols = G.cols g
      vecSize = numRows * numCols * 4 -- 4 directions
  visited <- VUM.replicate vecSize False
  let (st', status') = toNext g st
      initialKey = posDirToIndex numCols (pos st) (dir st)
  when (status' == Running) $
    VUM.write visited initialKey True
  go visited numCols st' status'
  where
    go visited cols state status = do
      let key = posDirToIndex cols (pos state) (dir state)
      isVisited <- VUM.read visited key
      if isVisited
        then pure True
        else case status of
          OutOfBounds -> pure False
          Done -> do
            let newState = state {dir = nextDir (dir state)}
            go visited cols newState Running
          Running -> do
            let (nextState, nextStatus) = toNext g state
            when (nextStatus == Running) $
              VUM.write visited key True
            go visited cols nextState nextStatus

part1 :: T.Text -> Int
part1 s =
  let (g, st) = parse s
      xs = traverseGridPart1 g st [pos st]
   in length $ HashSet.fromList xs

part2 :: T.Text -> Int
part2 s =
  let (g, st) = parse s
      candidates = HashSet.toList . HashSet.fromList $ traverseGridPart1 g st [pos st]
      testCandidate cand = runST $ do
        let numRows = G.rows g
            numCols = G.cols g
        vec <- VU.thaw (G.vals g)
        let idx = G.posToIdx cand numCols
        VUM.write vec idx '#'
        g' <- G.Grid <$> VU.freeze vec <*> pure numRows <*> pure numCols
        return $ fromEnum $ traverseGridPart2 g' st
   in sum $ parMap rseq testCandidate candidates

{- in sum $ withStrategy (parListChunk 10 rseq) $ map testCandidate candidates -}

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
