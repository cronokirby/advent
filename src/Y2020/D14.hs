{-# LANGUAGE LambdaCase #-}

module Y2020.D14 (problem) where

import qualified Advent as A
import Data.Bits (shiftL, testBit, (.&.), (.|.), setBit)
import Data.Char (isDigit)
import qualified Data.Map as Map
import Ourlude
import Text.ParserCombinators.ReadP as P

data Instruction
  = Mask Word64 Word64
  | Write Word64 Word64
  deriving (Eq, Show)

type Input = [Instruction]

data MaskType = OnMask | OffMask deriving (Eq, Show)

readMask :: MaskType -> String -> Word64
readMask x = map convert >>> foldl' go 0
  where
    placeholder = case x of
      OnMask -> 0
      OffMask -> 1

    convert :: Char -> Word64
    convert '1' = 1
    convert '0' = 0
    convert 'X' = placeholder
    convert c = error ("Unexpected " <> show c)

    go acc x = shiftL acc 1 .|. x

readInput :: Text -> Maybe Input
readInput = lines >>> traverse get
  where
    get = toString >>> P.readP_to_S instruction >>> viaNonEmpty (head >>> fst)

    word64 :: P.ReadP Word64
    word64 = do
      Just x <- readMaybe <$> P.many1 (P.satisfy isDigit)
      return x

    instruction :: P.ReadP Instruction
    instruction = (mask <|> write) <* P.eof

    write :: P.ReadP Instruction
    write = do
      void (P.string "mem[")
      addr <- word64
      void (P.string "] = ")
      val <- word64
      P.eof
      return (Write addr val)

    mask :: P.ReadP Instruction
    mask = do
      void (P.string "mask = ")
      maskString <- bitmasks
      let on = readMask OnMask maskString
          off = readMask OffMask maskString
      return (Mask on off)

    bitmasks :: P.ReadP String
    bitmasks = P.many1 (P.satisfy (`elem` ("01X" :: String)))

type Output1 = Word64

type TheState = (Map.Map Word64 Word64, (Word64, Word64))

solve1 :: Input -> Output1
solve1 = foldl' go (Map.empty, (0, 0xFFFFFFFFFF)) >>> fst >>> Map.elems >>> sum
  where
    go :: TheState -> Instruction -> TheState
    go (mp, (on, off)) = \case
      Mask on' off' -> (mp, (on', off'))
      Write addr val ->
        let masked = (val .|. on) .&. off
         in (Map.insert addr masked mp, (on, off))

type Output2 = Word64

solve2 :: Input -> Output1
solve2 = foldl' go (Map.empty, (0, 0xFFFFFFFFFF)) >>> fst >>> Map.elems >>> sum
  where
    getAddrs :: Word64 -> Word64 -> Word64 -> [Word64]
    getAddrs addr on off = traverse getFuncs [0..35] |> map (foldr (>>>) id >>> (addr |>))
      where
        getFuncs :: Int -> [Word64 -> Word64]
        getFuncs i = case (on `testBit` i, off `testBit` i) of
          (False, True) -> [(`setBit` i), id]
          (True, True) -> [(`setBit` i)]
          (False, False) | addr `testBit` i -> [(`setBit` i)]
          (_, _) -> [id]
    go :: TheState -> Instruction -> TheState
    go (mp, (on, off)) = \case
      Mask on' off' -> (mp, (on', off'))
      Write addr val ->
        let addrs = getAddrs addr on off
         in (foldl' (\acc a -> Map.insert a val acc) mp addrs, (on, off))

theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-14.txt" [] [] [] [] (A.ProblemInfo "TODO" 2020 14)
