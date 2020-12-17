{-# LANGUAGE RecordWildCards #-}

module Y2020.D17 (problem) where

import qualified Advent as A
import Data.List ((!!))
import qualified Data.Set as Set
import Ourlude

type Pos = (Int, Int, Int, Int)

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
      gridActive = Set.fromList [(x, y, 0, 0) | (y, line) <- zip [0 ..] theLines, (x, c) <- zip [0 ..] line, c == '#']
      (gridMin, gridMax) = setBounds gridActive
  Just Grid {..}

setBounds :: Set Pos -> (Pos, Pos)
setBounds set =
  let (minX, minY, minZ, minW) = Set.findMin set
      (maxX, maxY, maxZ, maxW) = Set.findMax set
      extra = 12
   in ((minX - extra, minY - extra, minZ - extra, minW - extra), (maxX + extra, maxY + extra, maxZ + extra, minW + extra))

step :: Grid -> Grid
step Grid {..} = Grid newMin newMax newActive
  where
    closePositions :: Pos -> [Pos]
    closePositions (posX, posY, posZ, posW) = [(posX + x, posY + y, posZ + z, posW + w) | x <- [-1 .. 1], y <- [-1 .. 1], z <- [-1 .. 1], w <- [-1 .. 1], (x, y, z, w) /= (0, 0, 0, 0)]

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
      let (minX, minY, minZ, minW) = gridMin
          (maxX, maxY, maxZ, maxW) = gridMax
       in [(x, y, z, w) | x <- [minX .. maxX], y <- [minY .. maxY], z <- [minZ .. maxZ], w <- [minW .. maxW]]

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

type Output2 = Int

solve2 :: Input -> Output2
solve2 = solve1

theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-17.txt" [] [] [] [] (A.ProblemInfo "TODO" 2020 17)
