{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day03 where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Char (isDigit)
import Data.FileEmbed (embedFile)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Util (Solution (..))

parseMulPairs :: ByteString -> [(Int, Int)]
parseMulPairs text = go text 0 []
  where
    go :: ByteString -> Int -> [(Int, Int)] -> [(Int, Int)]
    go bs pos acc
      | pos + 4 > BS.length bs = reverse acc
      | BS.take 4 (BS.drop pos bs) == "mul(" =
          let (pair, newPos) = extractPair bs (pos + 4)
           in case pair of
                Just (n1, n2) -> go bs newPos ((n1, n2) : acc)
                Nothing -> go bs newPos acc
      | otherwise = go bs (pos + 1) acc

    extractPair :: ByteString -> Int -> (Maybe (Int, Int), Int)
    extractPair bs start =
      let (num1, pos1) = readNum bs start
          (commaPos, hasComma) =
            if pos1 < BS.length bs && BS.index bs pos1 == fromIntegral (fromEnum ',')
              then (pos1 + 1, True)
              else (pos1, False)
          (num2, pos2) = if hasComma then readNum bs commaPos else (Nothing, pos1)
          (endPos, isValid) =
            if pos2 < BS.length bs && BS.index bs pos2 == fromIntegral (fromEnum ')')
              then (pos2 + 1, True)
              else (pos2, False)
       in case (num1, num2, isValid) of
            (Just n1, Just n2, True) -> (Just (n1, n2), endPos)
            _ -> (Nothing, endPos)

    readNum :: ByteString -> Int -> (Maybe Int, Int)
    readNum bs pos
      | pos >= BS.length bs || not (isDigit (toEnum (fromIntegral (BS.index bs pos)))) = (Nothing, pos)
      | otherwise =
          let (digits, _rest) = BS.span (isDigit . toEnum . fromIntegral) (BS.drop pos bs)
           in (Just (read (BC.unpack digits)), pos + BS.length digits)

removeDonts :: ByteString -> ByteString
removeDonts text = BS.toStrict $ B.toLazyByteString $ go text 0 True mempty
  where
    go :: ByteString -> Int -> Bool -> B.Builder -> B.Builder
    go bs pos isDo acc
      | pos >= BS.length bs = acc
      | isDo && pos + 7 <= BS.length bs && BS.take 7 (BS.drop pos bs) == "don't()" =
          go bs (pos + 7) False acc
      | not isDo && pos + 4 <= BS.length bs && BS.take 4 (BS.drop pos bs) == "do()" =
          go bs (pos + 4) True acc
      | isDo = go bs (pos + 1) isDo (acc <> B.word8 (BS.index bs pos))
      | otherwise = go bs (pos + 1) isDo acc

part1 :: T.Text -> Int
part1 text = sum $ map (uncurry (*)) $ parseMulPairs $ TE.encodeUtf8 text

part2 :: T.Text -> Int
part2 text = sum $ map (uncurry (*)) $ parseMulPairs $ removeDonts $ TE.encodeUtf8 text

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
exampleText2 = T.pack "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)do()?mul(8,5))"