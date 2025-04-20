{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day07 where

import Control.Parallel.Strategies (parMap, rseq)
import Data.FileEmbed (embedFile)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Text.Heredoc (here)
import Util (Solution (..), parseNumUnsafe)

data Equation = Equation {res :: Int, vals :: [Int]} deriving (Show)

parse :: T.Text -> [Equation]
parse text = map parseLine $ T.lines (T.strip text)
  where
    parseLine line =
      case T.splitOn (T.pack ":") line of
        (r : v : _) -> Equation {res = parseNumUnsafe r, vals = getValues v}
        _ -> error "Invalid input"
    getValues s = map parseNumUnsafe $ T.splitOn (T.pack " ") $ T.strip s

concatInt :: Int -> Int -> Int
concatInt a b = a * calcBase b 1 + b
  where
    calcBase t v
      | t > 0 = calcBase (t `div` 10) (v * 10)
      | otherwise = v

unConcatInt :: Int -> Int -> Maybe Int
unConcatInt joined rightSide = result
  where
    calcDivisor check divisor
      | check >= 10 = calcDivisor (check `div` 10) (divisor * 10)
      | otherwise = divisor
    divisor' = calcDivisor rightSide 10
    result =
      if joined `mod` divisor' == rightSide
        then Just (joined `div` divisor')
        else Nothing

eval :: Int -> [Int] -> Bool -> Bool
eval target values isPart2 =
  case values of
    [_] -> False
    [v1, v2] -> v1 + v2 == target || v1 * v2 == target || (isPart2 && v1 `concatInt` v2 == target)
    _ -> isMul || isAdd || isConcat
      where
        val = last values
        values' = take (length values - 1) values
        isMul = target `mod` val == 0 && eval (target `div` val) values' isPart2
        isAdd = target >= val && eval (target - val) values' isPart2
        isConcat =
          isPart2
            && case unConcatInt target val of
              Just lhs -> eval lhs values' isPart2
              _ -> False

solve :: T.Text -> Bool -> Int
solve s isPart2 = sum $ parMap rseq processEquation $ parse s
  where
    processEquation eq = if eval (res eq) (vals eq) isPart2 then res eq else 0

solutionDay07 :: Solution
solutionDay07 =
  Solution
    { day = 07,
      input = textInput,
      solvePart1 = part1,
      solvePart2 = part2,
      expectedPart1 = 5837374519342,
      expectedPart2 = 492383931650959
    }

textInput :: T.Text
textInput = TE.decodeUtf8 $(embedFile "data/day_07.txt")

part1 :: T.Text -> Int
part1 s = solve s False

part2 :: T.Text -> Int
part2 s = solve s True

exampleText :: T.Text
exampleText =
  T.pack
    [here|190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
|]
