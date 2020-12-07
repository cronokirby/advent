{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Y2020.D7 where

import qualified Advent as A
import Data.Char (isDigit, isSpace)
import Ourlude
import Text.ParserCombinators.ReadP as P
import qualified Data.Set as Set
import qualified Data.Map as Map

type BagType = Text

data Bag = Bag BagType [(Int, BagType)] deriving (Eq, Show)

type Input = [Bag]

readInput :: Text -> Maybe Input
readInput = lines >>> traverse getBag
  where
    getBag = toString >>> P.readP_to_S bag >>> viaNonEmpty (head >>> fst)

    notSpace :: P.ReadP String
    notSpace = P.munch1 (not <<< isSpace)

    color :: P.ReadP Text
    color = do
      c1 <- notSpace
      _ <- char ' '
      c2 <- notSpace
      return (fromString (c1 <> c2))

    int :: P.ReadP Int
    int = do
      Just x <- readMaybe <$> P.munch1 isDigit
      return x

    containedPart :: P.ReadP (Int, Text)
    containedPart = do
      int' <- int
      _ <- char ' '
      color' <- color
      _ <- P.string " bag" <|> P.string " bags"
      return (int', color')

    noOtherBags :: P.ReadP [(Int, Text)]
    noOtherBags = [] <$ P.string "no other bags"

    bag :: P.ReadP Bag
    bag = do
      color' <- color
      _ <- P.string " bags contain "
      bags <- noOtherBags <|> P.sepBy1 containedPart (P.string ", ")
      _ <- P.string "."
      return (Bag color' bags)

type Output1 = Int

type State1 = Set.Set BagType

solve1 :: Input -> Output1
solve1 input =
  let mp :: Map.Map BagType [BagType]
      mp = input |> map (\(Bag typ contains) -> (typ, map snd contains)) |> Map.fromList

      children :: BagType -> [BagType]
      children k = Map.findWithDefault [] k mp

      hasGoldAlready :: BagType -> State State1 Bool
      hasGoldAlready k = gets (Set.member k)

      putGold :: BagType -> State State1 ()
      putGold k = modify' (Set.insert k)

      hasGold :: BagType -> State State1 Bool
      hasGold "shinygold" = return True
      hasGold typ = do
        hasGoldAlready' <- hasGoldAlready typ
        if hasGoldAlready'
          then return True
          else do
            childrenGold <- traverse hasGold (children typ)
            let iHaveGold = or childrenGold
            when iHaveGold (putGold typ)
            return iHaveGold

      go :: Bag -> State State1 ()
      go (Bag typ _) = void <| hasGold typ
                      
  in runState (traverse go input) Set.empty |> snd |> Set.size


type State2 = Map.Map BagType Int

type Output2 = Maybe Int

solve2 :: Input -> Output2
solve2 input =
    let mp :: Map.Map BagType [(Int, BagType)]
        mp = input |> map (\(Bag typ contains) -> (typ, contains)) |> Map.fromList

        children :: BagType -> [(Int, BagType)]
        children k = Map.findWithDefault [] k mp

        getCount :: BagType -> State State2 (Maybe Int)
        getCount k = gets (Map.lookup k)

        putCount :: BagType -> Int -> State State2 ()
        putCount k count = modify' (Map.insert k count)

        calculateCount :: BagType -> State State2 Int
        calculateCount typ = getCount typ >>= \case
          Just x -> return x
          Nothing -> do
            let recurse (n, child) = do
                  theirCount <- calculateCount child
                  return (n + n * theirCount)
            myCount <- sum <$> traverse recurse (children typ)
            putCount typ myCount
            return myCount

        go :: Bag -> State State2 ()
        go (Bag typ _) = void <| calculateCount typ

  in runState (traverse go input) Map.empty |> snd |> Map.lookup "shinygold"


theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-7.txt" [] [] [] [] (A.ProblemInfo "TODO" 2020 7)
