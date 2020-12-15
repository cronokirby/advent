{-# LANGUAGE BangPatterns #-}

module Y2020.D15 (problem) where

import qualified Advent as A
import Ourlude
import qualified Data.Map as Map
import qualified Data.Text as T

type Input = [Int]

readInput :: Text -> Maybe Input
readInput = T.splitOn "," >>> traverse (toString >>> readMaybe)

type Output1 = Int

data Spoken = SpokenOnce Int | SpokenAtLeastTwice Int Int

type State1 = (Int, [Int], Int, Map.Map Int Spoken)

getAt :: Int -> Input  -> Int
getAt target input = go (0, input, 420, Map.empty)
  where
    go :: State1 -> Int
    go (now, _, lastVal, _) | now == target = lastVal
    go (now, x : xs, _, mp) =
      let !newState = (now + 1, xs, x, Map.insert x (SpokenOnce now) mp)
      in go newState
    go (now, [], lastVal, mp) =
      let speak = case fromMaybe (error "last val not there") (Map.lookup lastVal mp) of
            SpokenOnce _ -> 0
            SpokenAtLeastTwice recently earlier -> recently - earlier
          see Nothing = Just (SpokenOnce now)
          see (Just (SpokenOnce earlier)) = Just (SpokenAtLeastTwice now earlier)
          see (Just (SpokenAtLeastTwice recently _)) = Just (SpokenAtLeastTwice now recently)
          !newState = (now + 1, [], speak, Map.alter see speak mp)
      in go newState

solve1 :: Input -> Output1
solve1 = getAt 2000

testCasesA :: [A.TestCase Input Output1]
testCasesA = [A.TestCase [0, 3, 6] 436, A.TestCase [1, 3, 2] 1, A.TestCase [2, 1, 3] 10]

type Output2 = Int

solve2 :: Input -> Output2
solve2 = getAt 30000000

theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-15.txt" [] [] testCasesA [] (A.ProblemInfo "TODO" 2020 15)
