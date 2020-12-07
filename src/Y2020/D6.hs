module Y2020.D6 (problem) where

import qualified Advent as A
import qualified Data.Set as Set
import Ourlude
import Data.List (foldr1)
import qualified Data.Text as T

newtype Group = Group { gSets :: [Set.Set Char] } deriving (Eq, Show)

type Input = [Group]

readInput :: Text -> Maybe Input
readInput = T.splitOn "\n\n" >>> map readGroup >>> Just
  where
    readGroup = lines >>> map (toString >>> Set.fromList) >>> Group

type Output1 = Int

solve1 :: Input -> Int
solve1 = map (gSets >>> fold >>> Set.size) >>> sum

type Output2 = Int

solve2 :: Input -> Output2
solve2 = map (gSets >>> foldr1 Set.intersection >>> Set.size) >>> sum

theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-6.txt" [] [] [] [] (A.ProblemInfo "Custom Customs" 2020 6)
