{-# LANGUAGE GADTs #-}

module Grid where

import Data.Type.Equality ((:~:) (Refl))
import Data.Typeable (Typeable, eqT)
import Data.Vector qualified as V

newtype Pos = Pos (Int, Int) deriving (Show, Eq, Ord)

data Dir = N | NW | W | SW | S | SE | E | NE
  deriving (Show, Eq, Ord)

data Grid a = Grid
  { vals :: V.Vector a,
    rows :: Int,
    cols :: Int
  }
  deriving (Eq)

instance (Show a, Typeable a) => Show (Grid a) where
  show :: Grid a -> String
  show grid = unlines $ map unwords $ chunksOf (cols grid) $ map showElement $ V.toList $ vals grid
    where
      showElement :: a -> String
      showElement x = case eqT @a @Char of
        Just Refl -> [x] -- Display Char without quotes
        Nothing -> show x -- Use default show for other types

-- | Creates a grid with the specified number of rows and columns, filled with a given value.
--
-- * @numRows@: Number of rows (must be non-negative).
-- * @numCols@: Number of columns (must be non-negative).
-- * @value@: The value to fill the grid with.
-- * Returns: A new 'Grid' filled with @value@.
make :: Int -> Int -> a -> Grid a
make numRows numCols value =
  Grid
    { vals = V.replicate (numRows * numCols) value,
      rows = numRows,
      cols = numCols
    }

-- | Checks if a position is within the grid's bounds.
--
-- * @g@: The grid to check against.
-- * @pos@: The position to test.
-- * Returns: 'True' if @pos@ is inside the grid, 'False' otherwise.
isInside :: Grid a -> Pos -> Bool
isInside g (Pos (r, c)) = r >= 0 && r < rows g && c >= 0 && c < cols g

-- | Converts a position to a flat index in a row-major grid layout.
--
-- * @pos@: The position to convert.
-- * @cols@: Number of columns in the grid layout.
-- * Returns: The index corresponding to @pos@ in a grid with @cols@ columns.
posToIdx :: Pos -> Int -> Int
posToIdx (Pos (r, c)) cols = r * cols + c

-- | Converts a flat index to a position in a row-major grid layout.
--
-- * @idx@: The index to convert.
-- * @rows@: Number of rows in the grid (unused, kept for context).
-- * @cols@: Number of columns in the grid.
-- * Returns: The position corresponding to @idx@ in a grid with @cols@ columns.
idxToPos :: Int -> Int -> Int -> Pos
idxToPos idx _ cols = Pos (r, c)
  where
    r = idx `div` cols
    c = idx `mod` cols

-- | Retrieves the value at a position in the grid, if it exists.
--
-- * @grid@: The grid to query.
-- * @pos@: The position to access.
-- * Returns: 'Just' the value at @pos@ if within bounds, 'Nothing' otherwise.
get :: Grid a -> Pos -> Maybe a
get grid pos
  | isInside grid pos = Just (vals grid V.! idx)
  | otherwise = Nothing
  where
    idx = posToIdx pos (cols grid)

-- | Sets a value at a position in the grid, returning a new grid.
--
-- * @grid@: The original grid.
-- * @pos@: The position to update.
-- * @value@: The value to set.
-- * Returns: A new 'Grid' with @value@ at @pos@ if within bounds, unchanged otherwise.
set :: Grid a -> Pos -> a -> Grid a
set grid pos value
  | isInside grid pos = grid {vals = vals grid V.// [(idx, value)]}
  | otherwise = grid
  where
    idx = posToIdx pos (cols grid)

-- | Finds all positions in the grid where a predicate holds.
--
-- * @grid@: The grid to search.
-- * @p@: Predicate function to test each element.
-- * Returns: A list of positions where @p@ returns 'True' for the element.
findPositions :: Grid a -> (a -> Bool) -> [Pos]
findPositions grid p =
  [ idxToPos i (rows grid) (cols grid)
    | i <- [0 .. V.length (vals grid) - 1],
      p (vals grid V.! i)
  ]

-- | Moves a position in the specified direction.
--
-- * @pos@: The starting position.
-- * @dir@: The direction to move.
-- * Returns: The new position after moving in @dir@ (no bounds checking).
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

-- | Moves a position north.
--
-- * @pos@: The starting position.
-- * Returns: The new position after moving north.
north :: Pos -> Pos
north pos = move pos N

-- | Moves a position northwest.
--
-- * @pos@: The starting position.
-- * Returns: The new position after moving northwest.
northWest :: Pos -> Pos
northWest pos = move pos NW

-- | Moves a position west.
--
-- * @pos@: The starting position.
-- * Returns: The new position after moving west.
west :: Pos -> Pos
west pos = move pos W

-- | Moves a position southwest.
--
-- * @pos@: The starting position.
-- * Returns: The new position after moving southwest.
southWest :: Pos -> Pos
southWest pos = move pos SW

-- | Moves a position south.
--
-- * @pos@: The starting position.
-- * Returns: The new position after moving south.
south :: Pos -> Pos
south pos = move pos S

-- | Moves a position southeast.
--
-- * @pos@: The starting position.
-- * Returns: The new position after moving southeast.
southEast :: Pos -> Pos
southEast pos = move pos SE

-- | Moves a position east.
--
-- * @pos@: The starting position.
-- * Returns: The new position after moving east.
east :: Pos -> Pos
east pos = move pos E

-- | Moves a position northeast.
--
-- * @pos@: The starting position.
-- * Returns: The new position after moving northeast.
northEast :: Pos -> Pos
northEast pos = move pos NE

-- | Applies a function to the four cardinal neighbors (N, W, S, E) of a position.
--
-- * @g@: The grid to operate on.
-- * @pos@: The central position.
-- * @f@: Function to apply to each neighbor.
-- * Returns: A list of results from applying @f@ to the four neighbors.
apply4 :: Grid a -> Pos -> (Grid a -> Pos -> b) -> [b]
apply4 g pos f =
  [ f g (north pos),
    f g (west pos),
    f g (south pos),
    f g (east pos)
  ]

-- | Applies a function to all eight neighbors (N, NW, W, SW, S, SE, E, NE) of a position.
--
-- * @g@: The grid to operate on.
-- * @pos@: The central position.
-- * @f@: Function to apply to each neighbor.
-- * Returns: A list of results from applying @f@ to all eight neighbors.
apply8 :: Grid a -> Pos -> (Grid a -> Pos -> b) -> [b]
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

-- | Splits a list into chunks of specified size.
--
-- * @n@: Size of each chunk.
-- * @xs@: The list to split.
-- * Returns: A list of chunks, each of length @n@ (last chunk may be shorter).
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)