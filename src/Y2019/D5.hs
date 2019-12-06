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

data OPType = Mul | Add | Input | Output | JT | JF | SLT | SE | Halt deriving (Eq, Show)

data Mode = Imm | Pos deriving (Eq, Show)

data OP = OP OPType Mode Mode Mode deriving (Eq, Show)

decodeOP :: Int -> OP
decodeOP i =
    let (i1, opcode) = divMod i 100
        (i2, mode1) = divMod i1 10
        (i3, mode2) = divMod i2 10
        (_, mode3) = divMod i3 10
        o = OP (decodeType opcode) (mode mode1) (mode mode2) (mode mode3)
    in o
  where
    mode :: Int -> Mode
    mode 0 = Pos
    mode _ = Imm
    decodeType :: Int -> OPType
    decodeType 1 = Add
    decodeType 2 = Mul
    decodeType 3 = Input
    decodeType 4 = Output
    decodeType 5 = JT
    decodeType 6 = JF
    decodeType 7 = SLT
    decodeType 8 = SE
    decodeType _ = Halt

execute :: CodeM m => Int -> m ()
execute pc = do
    op <- read pc
    let (OP typ m1 m2 m3) = decodeOP op
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
        JT -> do
            a1 <- getValue m1 (pc + 1)
            a2 <- getValue m2 (pc + 2)
            let pc' = if a1 /= 0 then a2 else pc + 3
            execute pc'
        JF -> do
            a1 <- getValue m1 (pc + 1)
            a2 <- getValue m2 (pc + 2)
            let pc' = if a1 == 0 then a2 else pc + 3
            execute pc'
        SLT -> do
            a1 <- getValue m1 (pc + 1)
            a2 <- getValue m2 (pc + 2)
            o <- read (pc + 3)
            let w = if a1 < a2 then 1 else 0
            write o w
            execute (pc + 4)
        SE -> do
            a1 <- getValue m1 (pc + 1)
            a2 <- getValue m2 (pc + 2)
            o <- read (pc + 3)
            let w = if a1 == a2 then 1 else 0
            write o w
            execute (pc + 4)
        Halt -> return ()
  where
    getValue :: CodeM m => Mode -> Index -> m Int
    getValue Imm ix = read ix
    getValue Pos ix = read ix >>= read

type ConcreteCodeM s = ReaderT (MV.MVector s Int, STRef s [Int], Int) (ST.ST s)

instance CodeM (ConcreteCodeM s) where
    read ix = do
        (vec, _, _) <- ask
        lift $ MV.read vec ix
    write ix v = do
        (vec, _, _) <- ask
        lift $ MV.write vec ix v
    output v = do
        (_, outputs, _) <- ask
        lift $ modifySTRef' outputs (v:)
    input = asks (\(_, _, i) -> i)


runConcreteCode :: Int -> Input -> [Int]
runConcreteCode i input = ST.runST $ do
    vec <- V.unsafeThaw (V.fromList input)
    outputs <- newSTRef []
    runReaderT (execute 0 *> readOutputs) (vec, outputs, i)
  where
    readOutputs :: ConcreteCodeM s [Int]
    readOutputs = do
        (_, outputs, _) <- ask
        lift $ readSTRef outputs


solve1 :: Input -> Int
solve1 input = case runConcreteCode 1 input of
    (o:_) -> o
    _     -> error "No outputs"

solve2 :: Input -> Int
solve2 input = case runConcreteCode 5 input of
    (o:_) -> o
    _     -> error "No outputs"

theSolution :: A.Solution Input Int Int
theSolution = A.Solution readInput show show solve1 solve2

testsA :: [A.TestCase Input Int]
testsA = []

testsB :: [A.TestCase Input Int]
testsB = []

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2019-5.txt" [] [] testsA testsB (A.ProblemInfo "Sunny with a Chance of Asteroids" 2019 5)

