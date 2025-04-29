{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day09 where

import Data.Char (digitToInt)
import Data.FileEmbed (embedFile)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
-- import Data.Vector.Unboxed (Unbox)
-- import Data.Vector.Unboxed qualified as VU

import Debug.Trace (trace)
import Util (Solution (..))

data Hole = Hole {len :: Int, startIdx :: Int} deriving (Show)

data File = File {len :: Int, startIdx :: Int, fileId :: FileId, checkSum :: Int} deriving (Show)

data DiskState = DiskState {currHoleIdx :: Idx, currFileIdx :: Idx} deriving (Show)

type Idx = Int

type FileId = Int

parse :: T.Text -> (V.Vector Hole, V.Vector File)
parse t = (V.fromList holes', V.fromList files')
  where
    holes' = reverse $ filter (\h -> h.len > 0) holes
    files' = filter (\f -> f.len > 0) files
    (_, holes, files, _, _, _) = go t [] [] True 0 0

    go :: T.Text -> [Hole] -> [File] -> Bool -> Idx -> FileId -> (T.Text, [Hole], [File], Bool, Idx, FileId)
    go text hs fs isFile startIdx startFileId = case (T.uncons text, isFile) of
      (Just (c, rest), True) -> go rest hs (file : fs) False nextIdx nextFileId
        where
          (file, nextIdx, nextFileId) = processFileChar c startIdx startFileId
      (Just (c, rest), False) -> go rest (hole : hs) fs True nextIdx startFileId
        where
          (hole, nextIdx) = processHoleChar c startIdx
      _ -> (text, hs, fs, isFile, startIdx, startFileId)

processHoleChar :: Char -> Idx -> (Hole, Idx)
processHoleChar ch startIdx = (hole, nextIdx)
  where
    len = digitToInt ch
    nextIdx = startIdx + len
    hole = Hole {len = len, startIdx = startIdx}

processFileChar :: Char -> Idx -> FileId -> (File, Idx, FileId)
processFileChar ch startIdx fileId = (file, nextIdx, nextFileId)
  where
    len = digitToInt ch
    nextIdx = startIdx + len
    nextFileId = fileId + 1
    checkSum = sum [startIdx .. startIdx + len - 1] * fileId
    file = File {len = len, startIdx = startIdx, fileId = fileId, checkSum = checkSum}

nextHole :: V.Vector Hole -> Idx -> (V.Vector Hole, Idx, Idx)
nextHole holes holesIdx = (holes', nextHolesIdx, nextIdx)
  where
    hole = V.unsafeIndex holes holesIdx
    nextIdx = hole.startIdx
    hole' = Hole {len = hole.len - 1, startIdx = hole.startIdx + 1}
    holes' = V.unsafeUpdate holes (V.fromList [(holesIdx, hole')])
    nextHolesIdx = if hole'.len <= 0 then holesIdx + 1 else holesIdx

moveFile :: V.Vector File -> Idx -> Idx -> (V.Vector File, Idx)
moveFile files filesIdx holeIdx =
  if holeIdx <= file.startIdx + file.len
    then (files', nextFilesIdx)
    else (files, filesIdx)
  where
    file = V.unsafeIndex files filesIdx
    len = file.len - 1
    checkSum = file.fileId * (holeIdx - file.startIdx - len) + file.checkSum
    file' = File {len = len, startIdx = file.startIdx, fileId = file.fileId, checkSum = checkSum}
    files' = V.unsafeUpdate files (V.fromList [(filesIdx, file')])
    nextFilesIdx = if file'.len == 0 then filesIdx + 1 else filesIdx

showHoles :: V.Vector Hole -> String
showHoles = concatMap (\h -> show h ++ "\n")

showFiles :: V.Vector File -> String
showFiles = concatMap (\h -> show h ++ "\n")

solutionDay09 :: Solution
solutionDay09 =
  Solution
    { day = 09,
      input = textInput,
      solvePart1 = part1,
      solvePart2 = part2,
      expectedPart1 = 6310675819476,
      expectedPart2 = 6335972980679
    }

textInput :: T.Text
textInput = TE.decodeUtf8 $(embedFile "data/day_09.txt")

part1 :: T.Text -> Int
part1 s = solution
  where
    (holes, files) = parse s
    (_, finalFiles, _, _) = go holes files 0 0
    solution = V.foldl' (\acc f -> f.checkSum + acc) 0 finalFiles
    go :: V.Vector Hole -> V.Vector File -> Idx -> Idx -> (V.Vector Hole, V.Vector File, Idx, Idx)
    go hs0 fs0 holesIdx filesIdx =
      if nextHoleIdx >= nextFileIdx - 1
        then (hs1, fs1, holesIdx, filesIdx)
        else go hs1 fs1 holesIdx' filesIdx'
      where
        (hs1, holesIdx', nextHoleIdx) = nextHole hs0 holesIdx
        (fs1, filesIdx') = moveFile fs0 filesIdx nextHoleIdx
        nextFile = V.unsafeIndex fs1 filesIdx'
        nextFileIdx = nextFile.startIdx -- trace traceMsg $
        _traceMsg = "(len, fileId, checkSum) = " ++ show (nextHoleIdx, nextFile.startIdx, nextFile.len, nextFile.fileId, nextFile.checkSum)

part2 :: T.Text -> Int
part2 _s = 42

exampleText :: T.Text
exampleText = T.pack "2333133121414131402"
