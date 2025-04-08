{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day03 where

import Data.Char (isDigit)
import Data.FileEmbed (embedFile)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector.Strict qualified as V
import Util (Solution (..))

data ParseState = ParseState {pairs :: [(Int, Int)], current :: V.Vector Char, afterMul :: Bool, inNumbers :: Bool}

parseMulPairs :: String -> [(Int, Int)]
parseMulPairs text = pairs $ V.foldl' processChar (ParseState [] V.empty False False) (V.fromList text)
  where
    processChar :: ParseState -> Char -> ParseState
    processChar state c
      | c == 'l'
          && V.length curr >= 2
          && V.slice (V.length curr - 2) 2 curr == V.fromList ['m', 'u'] =
          state {afterMul = True, current = curr V.++ V.singleton c}
      | c == '(' && afterMul state =
          state {current = V.empty, afterMul = False, inNumbers = True}
      | c == ')' && inNumbers state =
          let nums = words $ map (\x -> if x == ',' then ' ' else x) $ V.toList (current state)
           in case nums of
                [n1, n2]
                  | all isDigit n1 && all isDigit n2 ->
                      state {pairs = pairs state ++ [(read n1, read n2)], current = V.empty, afterMul = False, inNumbers = False}
                _ -> state {current = V.empty, afterMul = False, inNumbers = False}
      | inNumbers state = state {current = current state V.++ V.singleton c}
      | otherwise = state {current = current state V.++ V.singleton c, afterMul = False}
      where
        curr = current state

data StateDont = StateDont {prev :: V.Vector Char, keep :: V.Vector Char, isDo :: Bool}

removeDonts :: String -> String
removeDonts input = V.toList $ keep $ V.foldl' processChar (StateDont V.empty V.empty True) (V.fromList input)
  where
    processChar :: StateDont -> Char -> StateDont
    processChar st char =
      let st1 = st {prev = prev st V.++ V.singleton char}
          st2 = if isDo st1 then st1 {keep = keep st1 V.++ V.singleton char} else st1
       in if char == ')'
            then
              let donts =
                    if V.length (prev st2) >= 7
                      then V.slice (V.length (prev st2) - 7) 7 (prev st2)
                      else V.empty
                  dos =
                    if V.length donts >= 4
                      then V.drop 3 donts
                      else V.empty
               in case (V.toList dos, V.toList donts) of
                    ("do()", _) -> st2 {isDo = True}
                    (_, "don't()") -> st2 {isDo = False}
                    _ -> st2
            else st2

part1 :: T.Text -> Int
part1 text = sum $ map (uncurry (*)) $ parseMulPairs $ T.unpack text

part2 :: T.Text -> Int
part2 text = sum $ map (uncurry (*)) $ parseMulPairs $ removeDonts $ T.unpack text

solutionDay03 :: Solution
solutionDay03 =
  Solution
    { day = 03,
      input = textInput,
      solvePart1 = part1,
      solvePart2 = part2,
      expectedPart1 = 169021493,
      expectedPart2 = 111762583
    }

textInput :: T.Text
textInput = TE.decodeUtf8 $(embedFile "data/day_03.txt")

exampleText1 :: T.Text
exampleText1 = T.pack "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

exampleText2 :: T.Text
exampleText2 = T.pack "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"