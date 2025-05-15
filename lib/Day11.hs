{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day11 where

import Data.FileEmbed (embedFile)
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Util (Solution (..), parseNumUnsafe)

type Label = Int

type Qty = Int

type StoneCounts = IntMap.IntMap Qty

parse :: T.Text -> StoneCounts
parse s = IntMap.fromList $ map (\t -> (parseNumUnsafe t, 1)) (T.words s)

blink :: StoneCounts -> Int -> StoneCounts
blink stoneCounts iters
  | iters == 0 = stoneCounts
  | otherwise = blink newDict (iters - 1)
  where
    newDict = IntMap.foldlWithKey' step IntMap.empty stoneCounts
    step accDict label qty =
      let alterValue possibleValue =
            case possibleValue of
              Nothing -> Just qty
              Just value -> Just (value + qty)
       in case label of
            0 -> IntMap.alter alterValue 1 accDict
            n ->
              if even digits
                then
                  let (l1, l2) = splitNumber n digits
                      d = IntMap.alter alterValue l1 accDict
                   in IntMap.alter alterValue l2 d
                else IntMap.alter alterValue (n * 2024) accDict
      where
        digits = digitCount label

digitCount :: Int -> Int
digitCount n
  | n == 0 = 1
  | otherwise = floor (logBase 10 (fromIntegral n :: Double)) + 1

splitNumber :: Int -> Int -> (Int, Int)
splitNumber n digits = (n `div` divisor, n `mod` divisor)
  where
    divisor = 10 ^ (digits `div` 2)

solutionDay11 :: Solution
solutionDay11 =
  Solution
    { day = 11,
      input = textInput,
      solvePart1 = part1,
      solvePart2 = part2,
      expectedPart1 = 239714,
      expectedPart2 = 284973560658514
    }

textInput :: T.Text
textInput = TE.decodeUtf8 $(embedFile "data/day_11.txt")

solve :: T.Text -> Int -> Int
solve s n = sum . IntMap.elems $ blink (parse s) n

part1 :: T.Text -> Int
part1 s = solve s 25

part2 :: T.Text -> Int
part2 s = solve s 75

exampleText :: T.Text
exampleText = T.pack "125 17"