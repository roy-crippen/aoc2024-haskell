{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day01 where

import Control.Monad.ST (runST)
import Data.FileEmbed (embedFile)
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Data.Vector.Algorithms.Intro qualified as VA
import Data.Vector.Unboxed qualified as V
import Text.Heredoc (here)
import Util (Solution (..))

parse :: T.Text -> (V.Vector Int, V.Vector Int)
parse inStr = (V.fromList xs, V.fromList ys)
  where
    (xs, ys) = unzip $ map parseLine ls
    ls = T.lines (T.strip inStr)
    parseLine line =
      case T.splitOn (T.pack "   ") line of
        [s1, s2] ->
          case (TR.decimal s1, TR.decimal s2) of
            (Right (x, _), Right (y, _)) -> (x, y)
            _ -> error "Invalid number string in input"
        _ -> error "Invalid input format: expected two numbers separated by three spaces"

solutionDay01 :: Solution
solutionDay01 =
  Solution
    { day = 1,
      input = textInput,
      solvePart1 = part1,
      solvePart2 = part2,
      expectedPart1 = 2086478,
      expectedPart2 = 24941624
    }

textInput :: T.Text
textInput = TE.decodeUtf8 $(embedFile "data/day_01.txt")

part1 :: T.Text -> Int
part1 s = solve $ parse s
  where
    solve (xs, ys) = V.sum $ V.zipWith (\x y -> abs (x - y)) (sortVector xs) (sortVector ys)
    sortVector xs = runST $ do
      v <- V.thaw xs
      VA.sort v
      V.freeze v

part2 :: T.Text -> Int
part2 s = freq $ parse s
  where
    freq (xs, ys) = V.foldl' (\acc x -> acc + x * IntMap.findWithDefault 0 x ysFreq) 0 xs
      where
        ysFreq = IntMap.fromListWith (+) $ V.toList $ V.map (\y -> (y, 1)) ys

exampleText :: T.Text
exampleText =
  T.pack
    [here|
3   4
4   3
2   5
1   3
3   9
3   3
|]