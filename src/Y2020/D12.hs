{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Y2020.D12 (problem) where

import qualified Advent as A
import Ourlude
import Data.List ((!!))

data Direction = N | S | E | W | L | R | F deriving (Eq, Show)

data Step = Step Direction Int deriving (Eq, Show)

type Input = [Step]

readInput :: Text -> Maybe Input
readInput = lines >>> traverse (toString >>> readStep)
  where
    readStep :: String -> Maybe Step
    readStep = \case
      [] -> Nothing
      (x : xs) -> do
        dir <- readDir x
        amount <- readMaybe xs
        return (Step dir amount)
    readDir :: Char -> Maybe Direction
    readDir = \case
      'N' -> Just N
      'S' -> Just S
      'E' -> Just E
      'W' -> Just W
      'L' -> Just L
      'R' -> Just R
      'F' -> Just F
      _ -> Nothing


data BoatState = BoatState
  { pos :: (Int, Int),
    heading :: Direction
  }
  deriving (Show)

manhattan :: (Int, Int) -> Int
manhattan (x, y) = abs x + abs y

addPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPos (x, y) (x', y') = (x + x', y + y')

rotateBy :: Direction -> Int -> Direction
rotateBy dir by = iterate next dir |> (!! (by `div` 90))
  where
    next = \case
      N -> E
      E -> S
      S -> W
      W -> N
      _ -> error "Unexpected direction in rotate"

type Output1 = Int

solve1 :: Input -> Output1
solve1 = foldl' go (BoatState (0, 0) E) >>> pos >>> manhattan
  where
    go :: BoatState -> Step -> BoatState
    go st@BoatState{..} (Step dir amount) = case dir of
      N -> BoatState (pos `addPos` (0, -amount)) heading
      S -> BoatState (pos `addPos` (0, amount)) heading
      W -> BoatState (pos `addPos` (-amount, 0)) heading
      E -> BoatState (pos `addPos` (amount, 0)) heading
      F -> go st (Step heading amount)
      L -> BoatState pos (heading `rotateBy` (360 - amount))
      R -> BoatState pos (heading `rotateBy` amount)


data BoatState2 = BoatState2
  { pos' :: (Int, Int),
    waypoint :: (Int, Int)
  }
  deriving (Show)
 
type Output2 = Int

scaleBy :: (Int, Int) -> Int -> (Int, Int)
scaleBy (x, y) a = (x * a, y * a)

rotatePos :: (Int, Int) -> Int -> (Int, Int)
rotatePos pos by = iterate next pos |> (!! (by `div` 90))
  where
    next (x, y) = (y, -x)

solve2 :: Input -> Output2
solve2 = foldl' go (BoatState2 (0, 0) (-10, -1)) >>> pos' >>> manhattan
  where
    go :: BoatState2 -> Step -> BoatState2
    go BoatState2{..} (Step dir amount) = case dir of
      F -> BoatState2 (pos' `addPos` (waypoint `scaleBy` amount)) waypoint
      N -> BoatState2 pos' (waypoint `addPos` (0, -amount))
      S -> BoatState2 pos' (waypoint `addPos` (0, amount))
      E -> BoatState2 pos' (waypoint `addPos` (-amount, 0))
      W -> BoatState2 pos' (waypoint `addPos` (amount, 0))
      R -> BoatState2 pos' (waypoint `rotatePos` amount)
      L -> BoatState2 pos' (waypoint `rotatePos` (360 - amount))

theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-12.txt" [] [] [] [] (A.ProblemInfo "Rain Risk" 2020 12)
