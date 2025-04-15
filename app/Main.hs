module Main where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Day01 (solutionDay01)
import Day02 (solutionDay02)
import Day03 (solutionDay03)
import Day04 (solutionDay04)
import Day05 (solutionDay05)
import Day06 (solutionDay06)
import Day99 (solutionDay99)
import Text.Printf (printf)
import Util (Solution (..))

solutions :: [Solution]
solutions =
  [ solutionDay01,
    solutionDay02,
    solutionDay03,
    solutionDay04,
    solutionDay05,
    solutionDay06,
    solutionDay99
  ]

main :: IO ()
main = mapM_ runSolution solutions
  where
    runSolution sol = do
      let in_str = input sol
      -- Time Part 1
      clockStart1 <- getCurrentTime
      v1 <- evaluate $ force $ solvePart1 sol in_str -- Force full evaluation of v1
      clockEnd1 <- getCurrentTime
      let clockElapsed1 = realToFrac (diffUTCTime clockEnd1 clockStart1) * 1000 -- Seconds to milliseconds

      -- Time Part 2
      clockStart2 <- getCurrentTime
      v2 <- evaluate $ force $ solvePart2 sol in_str -- Force full evaluation of v2
      clockEnd2 <- getCurrentTime
      let clockElapsed2 = realToFrac (diffUTCTime clockEnd2 clockStart2) * 1000 -- Seconds to milliseconds

      -- Format and print results
      let s1 = format (day sol) "part 1" v1 (expectedPart1 sol) clockElapsed1
      let s2 = format (day sol) "part 2" v2 (expectedPart2 sol) clockElapsed2
      putStrLn (s1 ++ "\n" ++ s2)

format :: Int -> String -> Int -> Int -> Double -> String
format dayNum part v expected clockElapsed = if v == expected then good else bad
  where
    dayStr = if dayNum < 10 then "0" ++ show dayNum else show dayNum
    vStr = replicate (15 - length (show v)) ' ' ++ show v
    clockTimeStr = printf "%8.4f" clockElapsed ++ " ms"
    base = "day " ++ dayStr ++ " " ++ part ++ " "
    good = base ++ vStr ++ " [" ++ clockTimeStr ++ "]"
    bad = base ++ "expected " ++ show expected ++ " but got " ++ vStr ++ " [" ++ clockTimeStr ++ "]"