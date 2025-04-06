module Main where

import Control.DeepSeq (deepseq)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Day01 (solutionDay01)
import Day02 (solutionDay02)
import Day99 (solutionDay99)
import Text.Printf (printf)
import Util (Solution (..))

solutions :: [Solution]
solutions = [solutionDay01, solutionDay02, solutionDay99]

main :: IO ()
main = mapM_ runSolution solutions
  where
    runSolution sol = do
      -- Time Part 1
      clockStart1 <- getCurrentTime
      let v1 = solvePart1 sol (input sol)
      deepseq v1 (return ()) -- Force full evaluation of v1
      clockEnd1 <- getCurrentTime
      let clockElapsed1 = realToFrac (diffUTCTime clockEnd1 clockStart1) * 1000000 -- Seconds to microseconds

      -- Time Part 2
      clockStart2 <- getCurrentTime
      let v2 = solvePart2 sol (input sol)
      deepseq v2 (return ()) -- Force full evaluation of v2
      clockEnd2 <- getCurrentTime
      let clockElapsed2 = realToFrac (diffUTCTime clockEnd2 clockStart2) * 1000000 -- Seconds to microseconds

      -- Format and print results
      let s1 = format (day sol) "part 1" v1 (expectedPart1 sol) clockElapsed1
      let s2 = format (day sol) "part 2" v2 (expectedPart2 sol) clockElapsed2
      putStrLn (s1 ++ "\n" ++ s2)

format :: Int -> String -> Int -> Int -> Double -> String
format day part v expected clockElapsed = if v == expected then good else bad
  where
    dayStr = if day < 10 then "0" ++ show day else show day
    vStr = replicate (15 - length (show v)) ' ' ++ show v
    clockTimeStr = printf "%8.1f" clockElapsed ++ " μs"
    base = "day " ++ dayStr ++ " " ++ part ++ " "
    good = base ++ vStr ++ " [" ++ clockTimeStr ++ "]"
    bad = base ++ "expected " ++ show expected ++ " but got " ++ vStr ++ " [" ++ clockTimeStr ++ "]"