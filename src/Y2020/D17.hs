{-# LANGUAGE RecordWildCards #-}

module Y2020.D17 (problem) where

import qualified Advent as A
import Data.List ((!!))
import qualified Data.Set as Set
import Ourlude

type Pos = (Int, Int, Int)

data Grid = Grid
  { gridMin :: Pos,
    gridMax :: Pos,
    gridActive :: Set Pos
  }
  deriving (Eq, Show)

type Input = Grid

readInput :: Text -> Maybe Input
readInput txt = do
  let theLines = lines txt |> map toString
      gridActive = Set.fromList [(x, y, 0) | (y, line) <- zip [0 ..] theLines, (x, c) <- zip [0 ..] line, c == '#']
      (gridMin, gridMax) = setBounds gridActive
  Just Grid {..}

setBounds :: Set (Int, Int, Int) -> ((Int, Int, Int), (Int, Int, Int))
setBounds set =
  let (minX, minY, minZ) = Set.findMin set
      (maxX, maxY, maxZ) = Set.findMax set
   in ((minX - 1, minY - 1, minZ - 1), (maxX + 1, maxY + 1, maxZ + 1))

step :: Grid -> Grid
step Grid {..} = Grid newMin newMax newActive
  where
    closePositions :: Pos -> [Pos]
    closePositions (posX, posY, posZ) = [(posX + x, posY + y, posZ + z) | x <- [-1 .. 1], y <- [-1 .. 1], z <- [-1 .. 1], (x, y, z) /= (0, 0, 0)]

    neighborCount :: Pos -> Int
    neighborCount pos = closePositions pos |> filter (`Set.member` gridActive) |> length

    willBeActive :: Pos -> Bool
    willBeActive pos = case (Set.member pos gridActive, neighborCount pos) of
      (True, 2) -> True
      (True, 3) -> True
      (True, _) -> False
      (False, 3) -> True
      (False, _) -> False

    cells :: [Pos]
    cells =
      let (minX, minY, minZ) = gridMin
          (maxX, maxY, maxZ) = gridMax
       in [(x, y, z) | x <- [minX .. maxX], y <- [minY .. maxY], z <- [minZ .. maxZ]]

    newActive :: Set Pos
    newActive = cells |> filter willBeActive |> Set.fromList

    (newMin, newMax) = setBounds newActive

activeCount :: Grid -> Int
activeCount Grid {..} = Set.size gridActive

type Output1 = Int

solve1 :: Input -> Output1
solve1 grid =
  let states = iterate step grid
   in states !! 6 |> activeCount

type Output2 = ()

solve2 :: Input -> Output2
solve2 _ = ()

theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/input-2020-17-A-0.txt" [] [] [] [] (A.ProblemInfo "TODO" 2020 17)
