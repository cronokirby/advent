module Y2020.D10 (problem) where

import qualified Advent as A
import Data.List (maximum)
import Data.Map.Lazy ((!))
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Ourlude

type Input = [Int]

readInput :: Text -> Maybe Input
readInput = lines >>> traverse (toString >>> readMaybe)

children :: Int -> [Int]
children x = map (+ x) [1 .. 3]

chain :: [Int] -> [Int]
chain input = go 0 (Set.fromList input) [] |> reverse
  where
    go current set acc
      | Set.null set = (current + 3) : current : acc
      | otherwise =
        let Just smallest = find (`Set.member` set) (children current)
         in go smallest (Set.delete smallest set) (current : acc)

type Output1 = Int

solve1 :: Input -> Output1
solve1 input =
  let chained = chain input
      differences = zipWith (-) (drop 1 chained) chained
      count n = filter (== n) >>> length
   in count 1 differences * count 3 differences

testCasesA :: [A.TestCase Input Output1]
testCasesA = [A.TestCase [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4] 35]

type Output2 = Integer

solve2 :: Input -> Output2
solve2 input =
  let max = maximum input
      fullInput = max : 0 : input
      mp = Map.fromList [((x, y), paths x y) | x <- fullInput, y <- fullInput]
        where
          paths x y
            | x == y = 1
            | otherwise = children x |> map (\k -> Map.findWithDefault 0 (k, y) mp) |> sum
   in mp ! (0, max)

testCasesB :: [A.TestCase Input Output2]
testCasesB =
  [ A.TestCase [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4] 8,
    A.TestCase [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3] 19208
  ]

theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-10.txt" [] [] testCasesA testCasesB (A.ProblemInfo "TODO" 2020 10)
