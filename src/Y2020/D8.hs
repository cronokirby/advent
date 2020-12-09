module Y2020.D8 (problem) where

import qualified Advent as A
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Vector ((//))
import qualified Data.Vector as V
import Ourlude

data InstructionType = NOP | ACC | JMP deriving (Eq, Show)

data Instruction = Instruction InstructionType Int deriving (Eq, Show)

type Input = [Instruction]

readInput :: Text -> Maybe Input
readInput = lines >>> traverse getInstruction
  where
    readInstr "nop" = Just NOP
    readInstr "acc" = Just ACC
    readInstr "jmp" = Just JMP

    getInstruction txt = case words txt of
      [instr, arg] -> do
        instr' <- readInstr instr
        arg' <- T.filter (/= '+') arg |> toString |> readMaybe
        return (Instruction instr' arg')
      _ -> Nothing

type Output1 = Int

solve1 :: Input -> Output1
solve1 input =
  let code = V.fromList input
      go i seen acc
        | Set.member i seen = acc
        | otherwise = case V.unsafeIndex code i of
          Instruction NOP _ -> go (i + 1) (Set.insert i seen) acc
          Instruction ACC n -> go (i + 1) (Set.insert i seen) (acc + n)
          Instruction JMP n -> go (i + n) (Set.insert i seen) acc
   in go 0 (Set.empty) 0

type Output2 = Maybe Int

solve2 :: Input -> Output2
solve2 input =
  let theCode = V.fromList input
      go code i seen acc
        | Set.member i seen = Nothing
        | i >= V.length code = Just acc
        | otherwise = case V.unsafeIndex code i of
          Instruction NOP _ -> go code (i + 1) (Set.insert i seen) acc
          Instruction ACC n -> go code (i + 1) (Set.insert i seen) (acc + n)
          Instruction JMP n -> go code (i + n) (Set.insert i seen) acc
      replaceAt i =
        let new = case V.unsafeIndex theCode i of
              Instruction NOP n -> Instruction JMP n
              Instruction JMP n -> Instruction NOP n
              Instruction ACC n -> Instruction ACC n
         in theCode // [(i, new)]
      doStuff i
        | i >= V.length theCode = Nothing
        | otherwise = case go (replaceAt i) 0 Set.empty 0 of
          Just acc -> Just acc
          Nothing -> doStuff (i + 1)
   in doStuff 0

theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-8.txt" [] [] [] [] (A.ProblemInfo "Handheld Halting" 2020 8)
