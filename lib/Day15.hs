{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day15 where

import Data.FileEmbed (embedFile)
import Data.List (elemIndex, foldl')
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector.Unboxed qualified as VU
import Grid qualified as G
import Text.Heredoc (here)
import Util (Solution (..))

type Moves = [G.Dir]

-- data State = State {pos :: G.Pos, dir :: G.Dir, rows :: Int, cols :: Int} deriving (Show)

parse :: T.Text -> (G.Grid Char, Moves, G.Pos)
parse s = (g, moves, startPos)
  where
    xs = T.lines s
    idx = case elemIndex T.empty xs of
      Just v -> v
      Nothing -> error "parse: could not find empty row"
    (ls, cs) = splitAt idx xs
    ls' = ls
    numRows = length ls'
    numCols = case ls' of
      [] -> error "parse: input must have at least one row"
      (l : _) -> T.length l
    dataVec = VU.fromList $ concatMap T.unpack ls'
    g = G.Grid {G.vals = dataVec, G.rows = numRows, G.cols = numCols}
    startPos = head $ G.findPositions g (== '@')
    -- state = State {pos = startPos, dir = head moves, rows = numRows, cols = numCols}
    moveChars = T.unpack . T.replace (T.pack "\n") (T.pack "") $ T.unlines cs
    moves =
      map
        ( \case
            '>' -> G.E
            '<' -> G.W
            '^' -> G.N
            _ -> G.S
        )
        moveChars

findMoves :: G.Dir -> G.Pos -> G.Grid Char -> Maybe [G.Pos]
findMoves dir pos g = case go pos [] of
  Just ls -> Just $ reverse ls
  _ -> Nothing
  where
    go :: G.Pos -> [G.Pos] -> Maybe [G.Pos]
    go p ls =
      let p' = G.move p dir
          ch = G.getUnsafe g p'
          ls' = p : ls
       in case ch of
            '#' -> Nothing
            c | c `elem` ['O', '@'] -> case go p' ls of
              Just zs -> Just (ls' ++ zs)
              _ -> Nothing
            _ -> Just ls'

processMoves :: G.Dir -> G.Pos -> Maybe [G.Pos] -> G.Grid Char -> (G.Pos, G.Grid Char)
processMoves dir guardPos positions g = case positions of
  Just ps -> (G.move guardPos dir, foldl' (\grid pos -> G.swapUnsafe grid pos (G.move pos dir)) g ps)
  _ -> (guardPos, g)

score :: G.Grid Char -> Int
score g = foldl' (\acc (G.Pos (r, c)) -> acc + 100 * r + c) 0 ps
  where
    ps = G.findPositions g (== 'O')

solutionDay15 :: Solution
solutionDay15 =
  Solution
    { day = 15,
      input = textInput,
      solvePart1 = part1,
      solvePart2 = part2,
      expectedPart1 = 1441031,
      expectedPart2 = 42
    }

textInput :: T.Text
textInput = TE.decodeUtf8 $(embedFile "data/day_15.txt")

-- add setManyUnsafe to Grid
-- change Grid to rord 'dot' record access
part1 :: T.Text -> Int
part1 s = score g'
  where
    (g, ms, guardPos) = parse s
    (_, g') = foldl' f (guardPos, g) ms
    f (guardP, grid) m = processMoves m guardP (findMoves m guardP grid) grid

part2 :: T.Text -> Int
part2 _s = 42

exampleText :: T.Text
exampleText =
  T.pack
    [here|##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^|]
