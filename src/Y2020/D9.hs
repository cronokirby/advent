module Y2020.D9 (problem) where

import qualified Advent as A
import Ourlude
import qualified Data.Set as Set
import Data.List (maximum, minimum)

type Input = [Int]

readInput :: Text -> Maybe Input
readInput = lines >>> traverse (toString >>> readMaybe)

type Output1 = Maybe Int

solve1 :: Input -> Output1
solve1 input = go input
  where
    step input = do
      x <- input !!? 25
      guard (not <| isSum (take 25 input) x)
      return x


    go [] = Nothing
    go (x : xs) = step (x : xs) <|> go xs

    isSum :: [Int] -> Int -> Bool
    isSum first25 x =
      let theSet = Set.fromList first25
      in any (\y -> Set.member (x - y) theSet) first25


type Output2 = Maybe Int

solve2 :: Input -> Output2
solve2 input = do
  target <- solve1 input
  span <- findSpan target input
  return (minimum span + maximum span)
  where
    findSpan :: Int -> [Int] -> Maybe [Int]
    findSpan target nums = go 0 [] nums
      where
        go _ _ [] = Nothing
        go 0 [] (x : xs)
          | x >= target = go 0 [] xs
          | otherwise = go x [x] xs
        go sm (a : as) (x : xs)
          | sm + x == target = Just (x : a : as)
          | sm + x > target = go (sm - a) as (x : xs)
          | otherwise = go (sm + x) (a : as ++ [x]) xs

theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-9.txt" [] [] [] [] (A.ProblemInfo "Encoding Error" 2020 9)
