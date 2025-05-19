{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day13 where

import Data.FileEmbed (embedFile)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Text.Heredoc (here)
import Util (Solution (..), chunkList, parseNumUnsafe)

type ABV = (Int, Int, Int) -- Type for equation ax + by = v

type XY = (Int, Int) -- Type for solution (x, y)

parse :: T.Text -> [(ABV, ABV)]
parse s = map parseAbv $ chunkList 3 $ filter (\t -> T.length t > 0) (T.lines s)
  where
    parseAbv :: [T.Text] -> (ABV, ABV)
    parseAbv xs = (abv1, abv2)
      where
        (s1, s2, s3) = case xs of
          [t1, t2, t3] -> (t1, t2, t3)
          _ -> error "Invalid input"
        (x1, y1) = parseValues s1 "+"
        (x2, y2) = parseValues s2 "+"
        (v1, v2) = parseValues s3 "="
        abv1 = (x1, x2, v1)
        abv2 = (y1, y2, v2)

parseValues :: T.Text -> String -> (Int, Int)
parseValues t s = case map (T.splitOn (T.pack s)) (T.splitOn (T.pack ",") t) of
  [[_, t1], [_, t2]] -> (parseNumUnsafe t1, parseNumUnsafe t2)
  _ -> error "Invalid input"

solveEquation :: (ABV, ABV) -> Int -> Maybe XY
solveEquation ((a1, b1, v1), (a2, b2, v2)) offset =
  let v1' = v1 + offset
      v2' = v2 + offset
      d = a1 * b2 - a2 * b1
      nx = v1' * b2 - v2' * b1
      ny = a1 * v2' - a2 * v1'
   in if d == 0
        then Nothing -- No unique solution (dependent or inconsistent)
        else -- Check if x and y are integers
          let (qx, rx) = nx `divMod` d
              (qy, ry) = ny `divMod` d
           in if rx == 0 && ry == 0
                then Just (qx, qy)
                else Nothing

solve :: (ABV, ABV) -> Int -> Int
solve equation offset = case solveEquation equation offset of
  Just (a, b) -> 3 * a + b
  Nothing -> 0

solutionDay13 :: Solution
solutionDay13 =
  Solution
    { day = 13,
      input = textInput,
      solvePart1 = part1,
      solvePart2 = part2,
      expectedPart1 = 29598,
      expectedPart2 = 93217456941970
    }

textInput :: T.Text
textInput = TE.decodeUtf8 $(embedFile "data/day_13.txt")

part1 :: T.Text -> Int
part1 s = sum $ map (`solve` 0) (parse s)

part2 :: T.Text -> Int
part2 s = sum $ map (`solve` 10000000000000) (parse s)

exampleText :: T.Text
exampleText =
  T.pack
    [here|Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279|]