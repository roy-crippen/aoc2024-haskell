{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day99 where

-- import Control.Monad.ST (runST)
import Data.FileEmbed (embedFile)
-- import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
-- import Data.Text.Read qualified as TR
-- import Data.Vector.Algorithms.Intro qualified as VA
-- import Data.Vector.Unboxed qualified as V
import Text.Heredoc (here)
import Util (Solution (..))

solutionDay99 :: Solution
solutionDay99 =
  Solution
    { day = 99,
      input = textInput,
      solvePart1 = part1,
      solvePart2 = part2,
      expectedPart1 = 42,
      expectedPart2 = 42
    }

textInput :: T.Text
textInput = TE.decodeUtf8 $(embedFile "data/day_99.txt")

part1 :: T.Text -> Int
part1 _s = 42

part2 :: T.Text -> Int
part2 _s = 42

exampleText :: T.Text
exampleText =
  T.pack
    [here|
|]