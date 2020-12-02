module Y2020.D1 (problem) where

import qualified Advent as A
import Ourlude

type Input = [Int]

readInput :: Text -> Maybe Input
readInput = lines >>> traverse (toString >>> readMaybe)

solve1 :: Input -> Int
solve1 input =
  (?: error "No Solution") <| viaNonEmpty head <| do
    x1 <- input
    x2 <- input
    guard (x1 + x2 == 2020)
    return (x1 * x2)

solve2 :: Input -> Int
solve2 input =
  (?: error "No Solution") <| viaNonEmpty head <| do
    x1 <- input
    x2 <- input
    x3 <- input
    guard (x1 + x2 + x3 == 2020)
    return (x1 * x2 * x3)

theSolution :: A.Solution Input Int Int
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-1.txt" [] [] [] [] (A.ProblemInfo "Report Repair" 2020 1)
