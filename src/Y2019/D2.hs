{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Y2019.D2
    ( problem
    )
where

import           Relude
import qualified Advent                        as A

import           Control.Monad.Loops            ( whileM_ )
import           Control.Monad.ST               ( runST )
import           Data.STRef
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as MV

type Input = [Int]

readInput :: Text -> Maybe Input
readInput = rightToMaybe . traverse readEither . T.splitOn ","

execNounVerb :: Input -> Int -> Int -> Int
execNounVerb input noun verb = runST $ do
    vec <- V.unsafeThaw (V.fromList input)
    MV.write vec 1 noun
    MV.write vec 2 verb
    step vec 0
    MV.read vec 0
  where
    opToFun op = case op of
        1 -> (+)
        2 -> (*)
        _ -> const
    step vec i = do
        op <- MV.read vec i
        if op == 99
            then return ()
            else do
                i1   <- MV.read vec (i + 1)
                i2   <- MV.read vec (i + 2)
                i3   <- MV.read vec (i + 3)
                arg1 <- MV.read vec i1
                arg2 <- MV.read vec i2
                MV.write vec i3 (opToFun op arg1 arg2)
                step vec (i + 4)

solve1 :: Input -> Int
solve1 input = execNounVerb input 12 2

solve2 :: Input -> Int
solve2 input = maybe (-1) (\(n, v) -> 100 * n + v) $ find
    (\(n, v) -> execNounVerb input n v == 19690720)
    [ (n, v) | n <- [0 .. 99], v <- [0 .. 99] ]

theSolution :: A.Solution Input Int Int
theSolution = A.Solution readInput show show solve1 solve2

testsA :: [A.TestCase Input Int]
testsA = []

testsB :: [A.TestCase Input Int]
testsB = []

problem :: A.Problem
problem = A.Problem theSolution
                    "data/prompt-2019-2.txt"
                    []
                    []
                    testsA
                    testsB
                    (A.ProblemInfo "1202 Program Alarm" 2019 2)
