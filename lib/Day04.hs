{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day04 where

import Data.FileEmbed (embedFile)
import Data.List (foldl')
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Grid qualified as G
import Text.Heredoc (here)
import Util (Solution (..))

-- | Parses a text representation into a 2D grid of characters.
--
-- * @text@: The input text, with rows separated by newlines.
-- * Returns: A 'G.Grid Char' with characters from the text in row-major order.
-- * Note: Assumes at least one non-empty row; errors if input is empty.
parse :: T.Text -> G.Grid Char
parse text =
  let ls = T.lines text -- Split into lines
      numRows = length ls -- Number of rows
      numCols = case ls of -- Number of columns from first row
        [] -> error "parse: input must have at least one row"
        (l : _) -> T.length l
      dataVec = V.fromList $ concatMap T.unpack ls -- Flatten to Vector Char
   in G.Grid
        { G.vals = dataVec,
          G.rows = numRows,
          G.cols = numCols
        }

-- | Checks if "MAS" appears in the specified direction from a position.
--
-- * @grid@: The grid to search in.
-- * @pos@: The starting position.
-- * @dir@: The direction to check.
-- * Returns: 'True' if "MAS" is found in @dir@ from @pos@, 'False' if out of bounds or not found.
isXmasInDir :: G.Grid Char -> G.Pos -> G.Dir -> Bool
isXmasInDir grid pos dir =
  let posM = G.move pos dir
      m = G.get grid posM
      posA = G.move posM dir
      a = G.get grid posA
      posS = G.move posA dir
      s = G.get grid posS
   in case (m, a, s) of
        (Just 'M', Just 'A', Just 'S') -> True
        _ -> False

-- | Counts the number of directions from a position where "MAS" appears.
--
-- * @grid@: The grid to search in.
-- * @pos@: The starting position.
-- * Returns: The number of directions (0-8) where "MAS" is found starting from @pos@.
xmasCount :: G.Grid Char -> G.Pos -> Int
xmasCount grid pos = length $ filter id xs
  where
    xs =
      [ isXmasInDir grid pos G.N,
        isXmasInDir grid pos G.NW,
        isXmasInDir grid pos G.W,
        isXmasInDir grid pos G.SW,
        isXmasInDir grid pos G.S,
        isXmasInDir grid pos G.SE,
        isXmasInDir grid pos G.E,
        isXmasInDir grid pos G.NE
      ]

-- | Checks if diagonals (NW-SE and NE-SW) from a position form 'M' and 'S' pairs.
--
-- * @grid@: The grid to search in.
-- * @pos@: The starting position.
-- * Returns: 'True' if both diagonal pairs are ('M', 'S') or ('S', 'M'), 'False' otherwise.
crossXmas :: G.Grid Char -> G.Pos -> Bool
crossXmas grid pos =
  let nw = G.get grid (G.northWest pos)
      se = G.get grid (G.southEast pos)
      nwSeOk = case (nw, se) of
        (Just 'M', Just 'S') -> True
        (Just 'S', Just 'M') -> True
        _ -> False
      ne = G.get grid (G.northEast pos)
      sw = G.get grid (G.southWest pos)
      neSwOk = case (ne, sw) of
        (Just 'M', Just 'S') -> True
        (Just 'S', Just 'M') -> True
        _ -> False
   in nwSeOk && neSwOk

-- | Counts total "MAS" occurrences in all directions from all 'X' positions.
--
-- * @input@: The text input to parse into a grid.
-- * Returns: The total count of "MAS" sequences found from all 'X' positions.
part1 :: T.Text -> Int
part1 input =
  let g = parse input
      xPositions = G.findPositions g (== 'X')
   in foldl' (\acc pos -> acc + xmasCount g pos) 0 xPositions

-- | Counts positions with 'A' where diagonals form 'M' and 'S' pairs.
--
-- * @input@: The text input to parse into a grid.
-- * Returns: The total count of 'A' positions with valid diagonal 'M' and 'S' pairs.
part2 :: T.Text -> Int
part2 input =
  let g = parse input
      aPositions = G.findPositions g (== 'A')
      validPositions = filter (crossXmas g) aPositions
   in length validPositions

solutionDay04 :: Solution
solutionDay04 =
  Solution
    { day = 04,
      input = textInput,
      solvePart1 = part1,
      solvePart2 = part2,
      expectedPart1 = 2573,
      expectedPart2 = 1850
    }

textInput :: T.Text
textInput = TE.decodeUtf8 $(embedFile "data/day_04.txt")

exampleText :: T.Text
exampleText =
  T.pack
    [here|MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
|]