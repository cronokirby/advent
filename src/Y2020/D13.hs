{-# LANGUAGE TupleSections #-}
module Y2020.D13 where

import qualified Advent as A
import Ourlude
import Data.List (minimumBy)
import qualified Data.Text as T

data Info = Info
  {
   ready :: Integer,
   buses :: [Maybe Integer]
  }
  deriving (Eq, Show)

type Input = Info

readInput :: Text -> Maybe Input
readInput txt =
  let [a, b] = lines txt
  in do
    ready <- toString a |> readMaybe
    let buses = T.splitOn "," b |> map (toString >>> readMaybe)
    return (Info ready buses)


type Output1 = Integer

solve1 :: Input -> Output1
solve1 (Info ready' buses') =
  let realBuses = catMaybes buses' |> sort
      canUseBus b = [1..] |> map (*b) |> find (>= ready')
      useTimes = realBuses |> map (\b -> (b,) <$> canUseBus b) |> catMaybes
      (bestBus, bestTime) = minimumBy (comparing snd) useTimes
  in (bestTime - ready') * bestBus

testCasesA :: [A.TestCase Input Output1]
testCasesA = [A.TestCase (Info 939 [Just 7, Just 13, Just 59, Just 31, Just 19]) 295]

type Output2 = Integer

bezout :: Integer -> Integer -> (Integer, Integer)
bezout a b = case a of
  0 -> (0, 1)
  _ ->
    let (x, y) = bezout (b `mod` a) a
    in (y - (b `div` a) * x, x)

fullMod :: Integer -> Integer -> Integer
fullMod x m = mod (mod x m + m) m

solveCong :: [(Integer, Integer)] -> Integer
solveCong [] = 0
solveCong [(a, n)] = a `fullMod` n
solveCong ((a1, n1) : (a2, n2) : rest) =
  let (m1, m2) = bezout n1 n2
      x = a1 * m2 * n2 + a2 * m1 * n1
  in solveCong ((x, n1 * n2) : rest)

solve2 :: Input -> Output2
solve2 (Info _ buses') =
  let congs = buses' |> zipWith (\i bus -> (-i,) <$> bus) [0..] |> catMaybes
  in solveCong congs

testCasesB :: [A.TestCase Input Output2]
testCasesB = [A.TestCase (Info 939 [Just 7, Just 13, Nothing, Nothing, Just 59, Nothing, Just 31, Just 19]) 1068781]

theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-13.txt" [] [] testCasesA testCasesB (A.ProblemInfo "TODO" 2020 13)
