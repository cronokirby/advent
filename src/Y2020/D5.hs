module Y2020.D5 (problem) where

import qualified Advent as A
import Data.List (maximum)
import Ourlude

data FB = F | B deriving (Eq, Show)

data LR = L | R deriving (Eq, Show)

data Seat = Seat [FB] [LR] deriving (Eq, Show)

type Input = [Seat]

readInput :: Text -> Maybe Input
readInput = lines >>> traverse makePair
  where
    fbFromC 'F' = F
    fbFromC 'B' = B
    lrFromC 'L' = L
    lrFromC 'R' = R

    makePair txt =
      let str = toString txt
          fbs = str |> filter (`elem` ("FB" :: String)) |> map fbFromC
          lrs = str |> filter (`elem` ("LR" :: String)) |> map lrFromC
       in Just (Seat fbs lrs)

type Output1 = Int

boolsToInt :: [Bool] -> Int
boolsToInt = foldl' (\acc x -> acc * 2 + (if x then 1 else 0)) 0

seatNumber :: Seat -> Int
seatNumber (Seat fbs lrs) =
  boolsToInt (map (== B) fbs ++ map (== R) lrs)

solve1 :: Input -> Output1
solve1 = map seatNumber >>> maximum

type Output2 = [Int]

solve2 :: Input -> Output2
solve2 seats =
  let numbers = map seatNumber seats
      sorted = sort numbers
  in [x + 1 | (x, y) <- zip sorted (viaNonEmpty tail sorted ?: []), x + 2 == y]

theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-5.txt" [] [] [] [] (A.ProblemInfo "Binary Boarding" 2020 5)
