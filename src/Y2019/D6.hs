{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Y2019.D6 where

import qualified Advent                       as A
import           Relude

import qualified Text.ParserCombinators.ReadP as P

type Label = Text

type Input = [(Label, Label)]

readInput :: Text -> Maybe Input
readInput = traverse get . lines
  where
    get = viaNonEmpty (fst . head) . P.readP_to_S pair . toString
    pair :: P.ReadP (Text, Text)
    pair = do
      t1 <- P.munch1 (/= ')')
      P.char ')'
      t2 <- P.munch1 (/= '\n')
      return (fromString t1, fromString t2)


data Orbits = Orbits Label [Orbits] deriving (Eq ,Show)

newtype OForest = OForest [Orbits] deriving (Eq, Show)

insert :: Label -> Label -> OForest -> OForest
insert target new (OForest []) = OForest [Orbits target [Orbits new []]]
insert target new (OForest (orbits:os)) = case go target new orbits of
    (orbits', True) -> OForest (orbits' : os)
    (_, False)      -> let OForest os' = insert target new (OForest os) in OForest (orbits : os')
  where
    go :: Label -> Label -> Orbits -> (Orbits, Bool)
    go target new (Orbits l children)
      | target == l = (Orbits l (Orbits new [] : children), True)
      | otherwise =
        let insertions = map (go target new) children
            children' = map fst insertions
            inserted = any snd insertions
        in (Orbits l children', inserted)

mkOrbits :: Input -> OForest
mkOrbits = foldl' (\acc (target, new) -> insert target new acc) (OForest [])

totalNodes :: Orbits -> Int
totalNodes (Orbits _ children) = 1 + sum (map totalNodes children)

totalOrbits :: Orbits -> Int
totalOrbits (Orbits _ children) = sum [totalNodes c + totalOrbits c | c <- children]

solve1 :: Input -> Int
solve1 = (\(OForest os) -> sum (map totalOrbits os)) . mkOrbits

solve2 :: Input -> Int
solve2 = solve1

theSolution :: A.Solution Input Int Int
theSolution = A.Solution readInput show show solve1 solve2

testsA :: [A.TestCase Input Int]
testsA = [A.TestCase [("R", "B"), ("B", "C"), ("C", "D"), ("D", "E"), ("E", "F"), ("B", "G"), ("G", "H"), ("D", "I"), ("E", "J"), ("J", "K"), ("K", "L")] 42]

testsB :: [A.TestCase Input Int]
testsB = []

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2019-6.txt" [] [] testsA testsB (A.ProblemInfo "Universal Orbit Map" 2019 6)

