{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Y2019.D1
  (
    problem
  )
where

import Relude
import qualified Advent as A

type Input = [Int]

readInput :: Text -> Maybe Input
readInput = mapM (rightToMaybe . readEither . toString) . lines

solve1 :: Input -> Int
solve1 = sum . map (\x -> div x 3 - 2)

solve2 :: Input -> Int
solve2 = sum . map fuel
  where
    fuel = maybe 0 sum . viaNonEmpty  tail . takeWhile (> 0) . iterate (\x -> div x 3 - 2)

theSolution :: A.Solution Input Int Int
theSolution = A.Solution readInput show show solve1 solve2

testsA :: [A.TestCase Input Int]
testsA = [A.TestCase [12] 2, A.TestCase [14] 2, A.TestCase [1969] 654]

testsB :: [A.TestCase Input Int]
testsB = [A.TestCase [14] 2, A.TestCase [1969] 966]

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2019-1.txt" [] [] testsA testsB (A.ProblemInfo "The Tyranny of the Rocket Equation" 2019 1)
