{-# LANGUAGE GADTs #-}

-- | A 2D grid data structure optimized for 'Char' or 'Int' values, using unboxed
-- vectors for fast access and updates. Provides efficient operations for grid
-- traversal, position manipulation, and neighbor queries, suitable for problems
-- like pathfinding or map processing (e.g., Advent of Code). All grid accesses
-- are bounds-checked unless specified, with unsafe indexing used internally for
-- performance.
module Grid where

import Data.Type.Equality ((:~:) (Refl))
import Data.Typeable (Typeable, eqT)
import Data.Vector.Unboxed (Unbox)
import Data.Vector.Unboxed qualified as VU

-- | A position on the grid, represented as a row and column pair @(row, col)@.
-- Rows increase downward, columns rightward, with (0,0) at the top-left.
newtype Pos = Pos (Int, Int)
  deriving (Show, Eq, Ord)

-- | A direction for grid movement, supporting eight-way navigation.
data Dir
  = -- | North (up)
    N
  | -- | Northwest
    NW
  | -- | West (left)
    W
  | -- | Southwest
    SW
  | -- | South (down)
    S
  | -- | Southeast
    SE
  | -- | East (right)
    E
  | -- | Northeast
    NE
  deriving (Show, Eq, Ord)

-- | A 2D grid of values, restricted to 'Char' or 'Int' for unboxed storage.
-- Stores elements in a row-major unboxed vector for fast access, with strict
-- dimensions to prevent thunks.
data Grid t = Grid
  { -- | Unboxed vector of grid values
    vals :: !(VU.Vector t),
    -- | Number of rows
    rows :: !Int,
    -- | Number of columns
    cols :: !Int
  }
  deriving (Eq)

-- | Pretty-prints the grid as a string, with rows separated by newlines.
-- 'Char' values are shown directly (e.g., '#'), 'Int' values use 'show'.
instance (Show t, Typeable t, Unbox t) => Show (Grid t) where
  show grid = unlines $ map unwords $ chunksOf (cols grid) $ map showElement $ VU.toList $ vals grid
    where
      showElement x = case eqT @t @Char of
        Just Refl -> [x]
        Nothing -> show x

-- | Creates a grid filled with a single value.
--
-- * @numRows@: Number of rows (must be non-negative).
-- * @numCols@: Number of columns (must be non-negative).
-- * @value@: Value to fill the grid (e.g., ' ' or 0).
--
-- Returns a 'Grid t' with dimensions @numRows x numCols@.
make :: (Unbox t) => Int -> Int -> t -> Grid t
make numRows numCols value =
  Grid
    { vals = VU.replicate (numRows * numCols) value,
      rows = numRows,
      cols = numCols
    }
{-# INLINE make #-}

-- | Checks if a position is within the grid's bounds.
--
-- * @grid@: The grid to check against.
-- * @pos@: The position to test.
--
-- Returns 'True' if @pos@ is valid (row and column are non-negative and less
-- than grid dimensions), 'False' otherwise.
isInside :: (Unbox t) => Grid t -> Pos -> Bool
isInside g (Pos (r, c)) = r >= 0 && r < rows g && c >= 0 && c < cols g
{-# INLINE isInside #-}

-- | Converts a position to a vector index (row-major order).
--
-- * @pos@: The position @(row, col)@.
-- * @cols@: Number of columns in the grid.
--
-- Returns the index for the unboxed vector. Assumes @pos@ is valid; use with
-- 'isInside' for safety.
posToIdx :: Pos -> Int -> Int
posToIdx (Pos (r, c)) cols = r * cols + c
{-# INLINE posToIdx #-}

-- | Converts a vector index to a position (row-major order).
--
-- * @idx@: The vector index.
-- * @rows@: Number of rows (unused, for API consistency).
-- * @cols@: Number of columns.
--
-- Returns the corresponding 'Pos'.
idxToPos :: Int -> Int -> Int -> Pos
idxToPos idx _ cols = Pos (r, c)
  where
    r = idx `div` cols
    c = idx `mod` cols
{-# INLINE idxToPos #-}

-- | Retrieves the value at a position, if valid.
--
-- * @grid@: The grid to query.
-- * @pos@: The position to access.
--
-- Returns 'Just' the value if @pos@ is within bounds, 'Nothing' otherwise.
-- Uses unsafe indexing internally for speed, guarded by 'isInside'.
get :: (Unbox t) => Grid t -> Pos -> Maybe t
get grid pos
  | isInside grid pos = Just (VU.unsafeIndex (vals grid) idx)
  | otherwise = Nothing
  where
    idx = posToIdx pos (cols grid)
{-# INLINE get #-}

-- | Sets the value at a position, if valid.
--
-- * @grid@: The grid to update.
-- * @pos@: The position to modify.
-- * @value@: The new value.
--
-- Returns a new 'Grid t' with the updated value if @pos@ is within bounds,
-- otherwise returns the original grid.
set :: (Unbox t) => Grid t -> Pos -> t -> Grid t
set grid pos value
  | isInside grid pos = grid {vals = vals grid VU.// [(idx, value)]}
  | otherwise = grid
  where
    idx = posToIdx pos (cols grid)
{-# INLINE set #-}

-- | Finds all positions where a predicate holds.
--
-- * @grid@: The grid to search.
-- * @p@: Predicate on grid values (e.g., @(== '#')@).
--
-- Returns a list of 'Pos' where the predicate is 'True'. Scans the entire grid.
findPositions :: (Unbox t) => Grid t -> (t -> Bool) -> [Pos]
findPositions grid p =
  [ idxToPos i (rows grid) (cols grid)
    | i <- [0 .. VU.length (vals grid) - 1],
      p (vals grid VU.! i)
  ]

-- | Moves a position in a given direction.
--
-- * @pos@: The starting position.
-- * @dir@: The direction to move.
--
-- Returns the new 'Pos' after applying the direction (e.g., 'N' decreases row).
-- Does not check bounds; use 'isInside' to validate the result.
move :: Pos -> Dir -> Pos
move (Pos (r, c)) dir = case dir of
  N -> Pos (r - 1, c)
  NW -> Pos (r - 1, c - 1)
  W -> Pos (r, c - 1)
  SW -> Pos (r + 1, c - 1)
  S -> Pos (r + 1, c)
  SE -> Pos (r + 1, c + 1)
  E -> Pos (r, c + 1)
  NE -> Pos (r - 1, c + 1)
{-# INLINE move #-}

-- | Moves a position north (up).
north :: Pos -> Pos
north pos = move pos N
{-# INLINE north #-}

-- | Moves a position northwest.
northWest :: Pos -> Pos
northWest pos = move pos NW
{-# INLINE northWest #-}

-- | Moves a position west (left).
west :: Pos -> Pos
west pos = move pos W
{-# INLINE west #-}

-- | Moves a position southwest.
southWest :: Pos -> Pos
southWest pos = move pos SW
{-# INLINE southWest #-}

-- | Moves a position south (down).
south :: Pos -> Pos
south pos = move pos S
{-# INLINE south #-}

-- | Moves a position southeast.
southEast :: Pos -> Pos
southEast pos = move pos SE
{-# INLINE southEast #-}

-- | Moves a position east (right).
east :: Pos -> Pos
east pos = move pos E
{-# INLINE east #-}

-- | Moves a position northeast.
northEast :: Pos -> Pos
northEast pos = move pos NE
{-# INLINE northEast #-}

-- | Applies a function to the four cardinal neighbors of a position.
--
-- * @grid@: The grid context.
-- * @pos@: The center position.
-- * @f@: Function to apply (e.g., 'get' for neighbor values).
--
-- Returns a list of results for north, west, south, and east neighbors, in that
-- order. Neighbors may be out of bounds; use 'isInside' or 'get' in @f@.
apply4 :: (Unbox t) => Grid t -> Pos -> (Grid t -> Pos -> b) -> [b]
apply4 g pos f =
  [ f g (north pos),
    f g (west pos),
    f g (south pos),
    f g (east pos)
  ]

-- | Applies a function to all eight neighbors of a position.
--
-- * @grid@: The grid context.
-- * @pos@: The center position.
-- * @f@: Function to apply (e.g., 'get' for neighbor values).
--
-- Returns a list of results for north, northwest, west, southwest, south,
-- southeast, east, and northeast neighbors, in that order. Neighbors may be out
-- of bounds; use 'isInside' or 'get' in @f@.
apply8 :: (Unbox t) => Grid t -> Pos -> (Grid t -> Pos -> b) -> [b]
apply8 g pos f =
  [ f g (north pos),
    f g (northWest pos),
    f g (west pos),
    f g (southWest pos),
    f g (south pos),
    f g (southEast pos),
    f g (east pos),
    f g (northEast pos)
  ]

-- | Splits a list into chunks of a given size.
--
-- * @n@: Size of each chunk.
-- * @xs@: List to split.
--
-- Returns a list of lists, each of length @n@ (last chunk may be shorter).
-- Used internally for 'show'.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)