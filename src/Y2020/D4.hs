module Y2020.D4 (problem) where

import qualified Advent as A
import Ourlude

type Input = ()

readInput :: Text -> Maybe Input
readInput _ = Just ()

type Output1 = ()

solve1 :: Input -> Output1
solve1 _ = ()

type Output2 = ()

solve2 :: Input -> Output2
solve2 _ = ()

theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-4.txt" [] [] [] [] (A.ProblemInfo "TODO" 2020 4)