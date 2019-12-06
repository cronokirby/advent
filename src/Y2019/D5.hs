{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Y2019.D5 where

import qualified Advent              as A
import           Relude

import qualified Control.Monad.ST    as ST
import           Data.STRef
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV

type Input = [Int]

readInput :: Text -> Maybe Input
readInput = rightToMaybe . traverse readEither . T.splitOn ","

type Index = Int

class Monad m => CodeM m where
    read :: Index -> m Int
    write :: Index -> Int -> m ()
    output :: Int -> m ()
    input :: m Int

data OPType = Mul | Add | Input | Output | Halt

data Mode = Imm | Pos

data OP = OP OPType Mode Mode Mode

decodeOP :: Int -> OP
decodeOP i =
    let (opcode, i1) = divMod i 100
        (mode1, i2) = divMod i1 10
        (mode2, i3) = divMod i2 10
        (mode3, _) = divMod i3 10
    in OP (decodeType opcode) (mode mode1) (mode mode2) (mode mode3)
  where
    mode :: Int -> Mode
    mode 0 = Pos
    mode _ = Imm
    decodeType :: Int -> OPType
    decodeType 1 = Add
    decodeType 2 = Mul
    decodeType 3 = Input
    decodeType 4 = Output
    decodeType _ = Halt

execute :: CodeM m => Int -> m ()
execute pc = do
    (OP typ m1 m2 m3) <- decodeOP <$> read pc
    case typ of
        Add -> do
            a1 <- getValue m1 (pc + 1)
            a2 <- getValue m2 (pc + 2)
            o <- read (pc + 3)
            write o (a1 + a2)
            execute (pc + 4)
        Mul -> do
            a1 <- getValue m1 (pc + 1)
            a2 <- getValue m2 (pc + 2)
            o <- read (pc + 3)
            write o (a1 * a2)
            execute (pc + 4)
        Input -> do
            v <- input
            o <- read (pc + 1)
            write o v
            execute (pc + 2)
        Output -> do
            a1 <- getValue m1 (pc + 1)
            output a1
            execute (pc + 2)
        Halt -> return ()
  where
    getValue :: CodeM m => Mode -> Index -> m Int
    getValue Imm ix = read ix
    getValue Pos ix = read ix >>= read

type ConcreteCodeM s = ReaderT (MV.MVector s Int, STRef s [Int]) (ST.ST s)

instance CodeM (ConcreteCodeM s) where
    read ix = do
        vec <- asks fst
        lift $ MV.read vec ix
    write ix v = do
        vec <- asks fst
        lift $ MV.write vec ix v
    output v = do
        outputs <- asks snd
        lift $ modifySTRef' outputs (v:)
    input = return 1


runConcreteCode :: Input -> [Int]
runConcreteCode input = ST.runST $ do
    vec <- V.unsafeThaw (V.fromList input)
    outputs <- newSTRef []
    runReaderT (execute 0 *> readOutputs) (vec, outputs)
  where
    readOutputs :: ConcreteCodeM s [Int]
    readOutputs = do
        outputs <- asks snd
        lift $ readSTRef outputs


solve1 :: Input -> Int
solve1 input = case runConcreteCode input of
    (o:_) -> o
    _ -> error "No outputs"

solve2 :: Input -> Int
solve2 = solve1

theSolution :: A.Solution Input Int Int
theSolution = A.Solution readInput show show solve1 solve2

testsA :: [A.TestCase Input Int]
testsA = []

testsB :: [A.TestCase Input Int]
testsB = []

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2019-5.txt" [] [] testsA testsB (A.ProblemInfo "Sunny with a Chance of Asteroids" 2019 5)

