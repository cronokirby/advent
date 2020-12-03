module Y2020.D3 (problem) where

import qualified Advent as A
import qualified Data.Set as Set
import Ourlude

type Pos = (Int, Int)

addPos :: Pos -> Pos -> Pos
addPos (x, y) (x', y') = (x + x', y + y')

data Grid = Grid
  { occupied :: Set.Set Pos,
    width :: Int,
    height :: Int
  }
  deriving (Eq, Show)

isInBounds :: Grid -> Pos -> Bool
isInBounds (Grid _ w h) (_, y) = y >= 0 && y < h

isOccupied :: Grid -> Pos -> Bool
isOccupied (Grid occupied' w _) (x, y) = Set.member (x `mod` w, y) occupied'

wouldEncounter :: Grid -> Pos -> Int
wouldEncounter grid step =
  iterate (addPos step) (0, 0)
    |> takeWhile (isInBounds grid)
    |> filter (isOccupied grid)
    |> length

readInput :: Text -> Maybe Grid
readInput txt = do
  let theLines = lines txt |> map toString
      theHeight = length theLines
  theWidth <- viaNonEmpty (head >>> length) theLines
  let positioned =
        zip [0 ..] theLines >>= \(y, line) -> zip [0 ..] line |> map (\(x, c) -> ((x, y), c))
      occupied' = positioned |> filter (snd >>> (== '#')) |> map fst |> Set.fromList
  Just (Grid occupied' theWidth theHeight)

solve1 :: Grid -> Int
solve1 grid = wouldEncounter grid (3, 1)

solve2 :: Grid -> Int
solve2 grid =
  [(3, 1), (1, 1), (5, 1), (7, 1), (1, 2)]
    |> map (wouldEncounter grid)
    |> product

theSolution :: A.Solution Grid Int Int
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-3.txt" [] [] [] [] (A.ProblemInfo "Toboggan Trajectory" 2020 3)
