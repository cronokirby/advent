{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Y2018.D1
  ( problem
  )
where

import           Control.Arrow                  ( (>>>) )
import qualified Data.HashMap.Strict           as HM

import           Relude

import qualified Advent                        as A


type Input = [Int]

readInput :: Text -> Maybe Input
readInput = mapM (rightToMaybe . readNum . toString) . lines
 where
  readNum :: String -> Either Text Int
  readNum ('-' : rest) = (\x -> -x) <$> readEither rest
  readNum ('+' : rest) = readEither rest
  readNum _            = Left "Expected - or +"


solve1 :: Input -> Int
solve1 = sum

solve2 :: Input -> Int
solve2 = cycle >>> scanl (+) 0 >>> scanl go (Right HM.empty) >>> headLeft
 where
  go
    :: Either Int (HM.HashMap Int Int) -> Int -> Either Int (HM.HashMap Int Int)
  go acc freq = do
    map <- acc
    let get = HM.lookupDefault 0 freq map + 1
    if get == 2 then Left freq else Right (HM.insert freq get map)
  headLeft (Left x : _ ) = x
  headLeft (_      : xs) = headLeft xs

theSolution :: A.Solution Input Int Int
theSolution = A.Solution readInput show show solve1 solve2

testFilesA :: [A.TestFile]
testFilesA =
  [A.TestFile "data/input-2018-1-A-0.txt" "data/output-2018-1-A-0.txt"]

testFilesB :: [A.TestFile]
testFilesB =
  [A.TestFile "data/input-2018-1-B-0.txt" "data/output-2018-1-B-0.txt"]

problem :: A.Problem
problem = A.Problem theSolution
                    "data/prompt-2019-1.txt"
                    testFilesA
                    testFilesB
                    [A.TestCase [1, 1, -2] 0]
                    [A.TestCase [3, 3, 4, -2, -4] 10]
                    (A.ProblemInfo "Chronal Calibration" 2018 1)
