{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Y2020.D15 (problem) where

import qualified Advent as A
import Control.Monad.ST as ST
import qualified Data.Text as T
import qualified Data.Vector.Unboxed.Mutable as MV
import Ourlude

type Input = [Int]

readInput :: Text -> Maybe Input
readInput = T.splitOn "," >>> traverse (toString >>> readMaybe)

type Output1 = Int

data Env s = Env (MV.STVector s (Int, Int)) Int Int

getAt :: Int -> Input -> Int
getAt target input = runST do
  arr <- MV.replicate target (-1, -1)
  forM_ (zip [0 ..] input) (\(i, x) -> MV.write arr x (i, -1))
  go (Env arr 420 (length input))
  where
    go :: Env s -> ST s Int
    go (Env _ lastVal now) | now == target = return lastVal
    go (Env arr lastVal now) = do
      speak <-
        MV.read arr lastVal >>= \case
          (-1, -1) -> return 0
          (_, -1) -> return 0
          (x, y) -> return (x - y)
      let change = \(x, _) -> (now, x)
      speak `seq` MV.modify arr change speak
      go (Env arr speak (now + 1))

solve1 :: Input -> Output1
solve1 = getAt 2020

testCasesA :: [A.TestCase Input Output1]
testCasesA = [A.TestCase [0, 3, 6] 436, A.TestCase [1, 3, 2] 1, A.TestCase [2, 1, 3] 10]

type Output2 = Int

solve2 :: Input -> Output2
solve2 = getAt 30000000

theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-15.txt" [] [] testCasesA [] (A.ProblemInfo "TODO" 2020 15)
