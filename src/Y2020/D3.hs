module Y2020.D3 (problem) where

import qualified Advent as A
import Ourlude

type Input = ()

readInput :: Text -> Maybe Input
readInput _ = Just ()

solve1 :: Input -> ()
solve1 _ = ()

solve2 :: Input -> ()
solve2 _ = ()

theSolution :: A.Solution Input () ()
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-2.txt" [] [] [] [] (A.ProblemInfo "TODO" 2020 3)