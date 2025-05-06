{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Day09 where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Char (digitToInt)
import Data.FileEmbed (embedFile)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Util (Solution (..))

data Hole = Hole {len :: Int, startIdx :: Int} deriving (Show)

data File = File {len :: Int, startIdx :: Int, fileId :: Int, checkSum :: Int} deriving (Show)

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

nextHole :: VM.MVector s Hole -> Idx -> ST s (Idx, Idx)
nextHole holes holesIdx = do
  hole <- VM.unsafeRead holes holesIdx
  let nextIdx = hole.startIdx
      hole' = Hole {len = hole.len - 1, startIdx = hole.startIdx + 1}
  VM.unsafeWrite holes holesIdx hole'
  let nextHolesIdx = if hole'.len <= 0 then holesIdx + 1 else holesIdx
  pure (nextHolesIdx, nextIdx)

moveFile :: VM.MVector s File -> Idx -> Idx -> ST s Idx
moveFile files filesIdx holeIdx = do
  file <- VM.unsafeRead files filesIdx
  if holeIdx <= file.startIdx + file.len
    then do
      let len = file.len - 1
          checkSum = file.fileId * (holeIdx - file.startIdx - len) + file.checkSum
          file' = File {len = len, startIdx = file.startIdx, fileId = file.fileId, checkSum = checkSum}
      VM.unsafeWrite files filesIdx file'
      pure $ if len == 0 then filesIdx + 1 else filesIdx
    else pure filesIdx

findNextHoleIdx :: File -> VM.MVector s Hole -> VM.MVector s Bool -> ST s (Maybe Int)
findNextHoleIdx file holes available = do
  let n = VM.length holes
  go 0 n
  where
    go i n
      | i >= n = pure Nothing
      | otherwise = do
          isAvailable <- VM.unsafeRead available i
          if isAvailable
            then do
              h <- VM.unsafeRead holes i
              if h.len >= file.len && h.startIdx < file.startIdx
                then pure (Just i)
                else go (i + 1) n
            else go (i + 1) n

fillHole :: Hole -> File -> (Hole, File)
fillHole hole file = (hole', file')
  where
    hole' = Hole {len = hole.len - file.len, startIdx = hole.startIdx + file.len}
    file' = file {startIdx = hole.startIdx, checkSum = checkSum}
    checkSum = file.fileId * (file.len * hole.startIdx + (file.len * (file.len - 1)) `div` 2)

showHoles :: V.Vector Hole -> String
showHoles = concatMap (\h -> show h ++ "\n")

showFiles :: V.Vector File -> String
showFiles = concatMap (\f -> show f ++ "\n")

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
part1 s = runST $ do
  let (holes, files) = parse s
  holesM <- V.thaw holes
  filesM <- V.thaw files
  go holesM filesM 0 0
  finalFiles <- V.freeze filesM
  pure $ V.foldl' (\acc f -> f.checkSum + acc) 0 finalFiles
  where
    go :: VM.MVector s Hole -> VM.MVector s File -> Idx -> Idx -> ST s ()
    go hs fs holesIdx filesIdx = do
      (holesIdx', nextHoleIdx) <- nextHole hs holesIdx
      filesIdx' <- moveFile fs filesIdx nextHoleIdx
      nextFile <- VM.unsafeRead fs filesIdx'
      let nextFileIdx = nextFile.startIdx
      if nextHoleIdx >= nextFileIdx - 1
        then pure ()
        else go hs fs holesIdx' filesIdx'

part2 :: T.Text -> Int
part2 s = runST $ do
  let (holes, files) = parse s
  holesM <- V.thaw holes
  filesM <- V.thaw files
  available <- VM.replicate (V.length holes) True
  go holesM filesM available 0 0
  finalFiles <- V.freeze filesM
  pure $ V.foldl' (\acc f -> f.checkSum + acc) 0 finalFiles
  where
    go :: VM.MVector s Hole -> VM.MVector s File -> VM.MVector s Bool -> Idx -> Idx -> ST s ()
    go hs fs available holesIdx filesIdx = do
      if filesIdx >= VM.length fs
        then pure ()
        else do
          file <- VM.unsafeRead fs filesIdx
          mHoleIdx <- findNextHoleIdx file hs available
          case mHoleIdx of
            Just hIdx -> do
              hole <- VM.unsafeRead hs hIdx
              let (hole', file') = fillHole hole file
              when (hole'.len > 0) $
                VM.unsafeWrite hs hIdx hole'
              when (hole'.len <= 0) $
                VM.unsafeWrite available hIdx False
              VM.unsafeWrite fs filesIdx file'
              let nextHolesIdx = if hole'.len <= 0 then hIdx + 1 else holesIdx
              go hs fs available nextHolesIdx (filesIdx + 1)
            Nothing -> go hs fs available holesIdx (filesIdx + 1)

exampleText :: T.Text
exampleText = T.pack "2333133121414131402"