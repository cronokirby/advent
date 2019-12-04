{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Y2019.D4
    ( problem
    )
where

import           Relude
import qualified Advent                        as A

import           Data.Foldable                  ( minimum )
import qualified Data.HashSet                  as Set
import qualified Data.Text                     as T
import qualified Text.ParserCombinators.ReadP  as P

data Input = Input { start :: Int, end :: Int } deriving (Eq, Show)

readInput :: Text -> Maybe Input
readInput = viaNonEmpty (fst . head) . P.readP_to_S go . toString
  where
    int :: P.ReadP Int
    int = P.readS_to_P reads
    go :: P.ReadP Input
    go = do
        start <- int
        P.char '-'
        end <- int
        P.eof
        return (Input { .. })

validWithCond :: Input -> ([Int] -> Bool) -> Int -> Bool
validWithCond Input {..} cond x =
    let digs = digits x
    in  inRange start end x && length digs == 6 && decreasing digs && cond digs
  where
    inRange :: Int -> Int -> Int -> Bool
    inRange start end x = start <= x && x <= end
    digits :: Int -> [Int]
    digits x | x < 10    = [x]
             | otherwise = mod x 10 : digits (div x 10)
    decreasing :: Ord o => [o] -> Bool
    decreasing xs = case xs of
        []           -> True
        [       _ ]  -> True
        x1 : x2 : xs -> x1 >= x2 && decreasing (x2 : xs)

valid1 :: Input -> Int -> Bool
valid1 input = validWithCond input existsPair
  where
    existsPair :: [Int] -> Bool
    existsPair digs = or (zipWith (==) digs (drop 1 digs))

countValid :: (Input -> Int -> Bool) -> Input -> Int
countValid valid input@Input {..} =
    length (filter (valid input) [start .. end])

solve1 :: Input -> Int
solve1 = countValid valid1

runLengths :: [Int] -> [Int]
runLengths []       = []
runLengths (x : xs) = go 1 x xs
  where
    go n t xs = case xs of
        []     -> [n]
        x : xs -> if x == t then go (n + 1) t xs else n : go 1 x xs

valid2 :: Input -> Int -> Bool
valid2 input = validWithCond input existsIsolatedPair
  where
    existsIsolatedPair :: [Int] -> Bool
    existsIsolatedPair = elem 2 . runLengths

solve2 :: Input -> Int
solve2 = countValid valid2

theSolution :: A.Solution Input Int Int
theSolution = A.Solution readInput show show solve1 solve2

testsA :: [A.TestCase Input Int]
testsA = []

testsB :: [A.TestCase Input Int]
testsB = []

problem :: A.Problem
problem = A.Problem theSolution
                    "data/prompt-2019-4.txt"
                    []
                    []
                    testsA
                    testsB
                    (A.ProblemInfo "Crossed Wires" 2019 4)
