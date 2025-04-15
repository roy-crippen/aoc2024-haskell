{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day05 where

import Data.FileEmbed (embedFile)
import Data.List (foldl', sortBy)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Data.Word (Word16)
import Text.Heredoc (here)
import Util (Solution (..))

-- | A directed graph mapping nodes to their successors.
type Rules = M.Map Word16 (S.Set Word16)

-- | Parses input into rules (graph edges) and updates (sequences).
--
-- * @input@: Text with rules and updates separated by a blank line.
-- * Returns: A pair of 'Rules' and a list of sequences.
-- * Note: Errors on invalid input (empty sections, malformed numbers).
parse :: T.Text -> (Rules, [[Word16]])
parse input =
  let sections = T.splitOn (T.pack "\n\n") input
      (sRules, sUpdates) = case sections of
        [r, u] -> (r, u)
        _ -> error "parse: expected two sections separated by blank line"
      pairs = map parsePair (T.lines sRules)
      rules = foldl' insertPair M.empty pairs
      updates = map parseUpdate (T.lines sUpdates)
   in (rules, updates)
  where
    parsePair line = case T.splitOn (T.pack "|") line of
      [s1, s2] -> case (TR.decimal s1, TR.decimal s2) of
        (Right (v1, _), Right (v2, _)) -> (v1, v2)
        _ -> error $ "parsePair: invalid numbers in " ++ T.unpack line
      _ -> error $ "parsePair: expected 'n|m' in " ++ T.unpack line
    insertPair acc (k, v) = M.alter (addToSet v) k acc
    addToSet v Nothing = Just (S.singleton v)
    addToSet v (Just s) = Just (S.insert v s)

    -- parse helper
    parseUpdate :: T.Text -> [Word16]
    parseUpdate line = map parseNum (T.splitOn (T.pack ",") line)
    parseNum s = case TR.decimal (T.strip s) of
      Right (n, rest) | T.null rest -> n
      _ -> error $ "parseNum: invalid number in " ++ T.unpack s

-- | Checks if a sequence respects the graph's topological order.
--
-- * @rules@: The graph of predecessor-successor relationships.
-- * @xs@: The sequence to check.
-- * Returns: 'True' if each element precedes its successors according to @rules@, 'False' otherwise.
isInOrder :: Rules -> [Word16] -> Bool
isInOrder rules xs =
  let pairs = zip xs (drop 1 xs)
   in foldl' (\acc (curr, next) -> acc && checkPair curr next) True pairs
  where
    checkPair curr next = case M.lookup curr rules of
      Just set -> S.member next set
      Nothing -> False

-- | Compares two nodes based on graph edges for sorting.
--
-- * @rules@: The graph of predecessor-successor relationships.
-- * @a@: First node.
-- * @b@: Second node.
-- * Returns: 'LT' if a precedes b, 'GT' if b precedes a, 'EQ' if neither.
cmpByRule :: Rules -> Word16 -> Word16 -> Ordering
cmpByRule rules a b = case (M.lookup a rules, M.lookup b rules) of
  (Nothing, _) -> GT
  (_, Nothing) -> LT
  (Just aSet, Just bSet) ->
    let bInA = S.member b aSet
        aInB = S.member a bSet
     in case (bInA, aInB) of
          (True, False) -> LT
          (False, True) -> GT
          _ -> EQ

-- | Counts the sum of middle elements from valid sequences.
--
-- * @input@: The text input to parse.
-- * Returns: The sum of middle elements from sequences that respect the graph's order.
part1 :: T.Text -> Int
part1 input =
  let (rules, updates) = parse input
      goods = filter (isInOrder rules) updates
      middle = \xs -> take 1 $ drop (length xs `div` 2) xs
      centers = concatMap middle goods
   in sum (map fromIntegral centers)  -- Convert Word16 to Int

-- | Counts the sum of middle elements from sorted invalid sequences.
--
-- * @input@: The text input to parse.
-- * Returns: The sum of middle elements from invalid sequences sorted by graph order.
part2 :: T.Text -> Int
part2 input =
  let (rules, updates) = parse input
      bads = filter (not . isInOrder rules) updates
      goods = map (sortBy (cmpByRule rules)) bads
      middle = \xs -> take 1 $ drop (length xs `div` 2) xs
      centers = concatMap middle goods
   in sum (map fromIntegral centers)  -- Convert Word16 to Int

solutionDay05 :: Solution
solutionDay05 =
  Solution
    { day = 05,
      input = textInput,
      solvePart1 = part1,
      solvePart2 = part2,
      expectedPart1 = 6034,
      expectedPart2 = 6305
    }

textInput :: T.Text
textInput = TE.decodeUtf8 $(embedFile "data/day_05.txt")

exampleText :: T.Text
exampleText =
  T.pack
    [here|47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
|]