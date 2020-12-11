{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Y2020.D11 (problem) where

import qualified Advent as A
import qualified Data.Map as Map
import Ourlude
import qualified Text.Show

data Cell = Empty | Occupied | Floor deriving (Eq)

instance Show Cell where
  show = \case
    Empty -> "L"
    Occupied -> "#"
    Floor -> "."

data Grid = Grid Int Int (Map.Map (Int, Int) Cell) deriving (Eq)

instance Show Grid where
  show (Grid w h g) =
    let cells = [fold [show <| Map.findWithDefault Floor (x, y) g | x <- [0 .. (w - 1)]] | y <- [0 .. (h - 1)]]
     in "Grid " <> show w <> " " <> show h <> "\n" <> intercalate "\n" cells

directions :: [(Int, Int)]
directions = [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0)]

neighbors :: Grid -> (Int, Int) -> [Cell]
neighbors (Grid _ _ mp) (x, y) =
  [(x + dx, y + dy) | (dx, dy) <- directions] |> map (`Map.lookup` mp) |> catMaybes

step :: Grid -> Grid
step grid@(Grid w h mp) = Grid w h (Map.mapWithKey stepCell mp)
  where
    count target = filter (== target) >>> length

    occupied pos = count Occupied (neighbors grid pos)

    stepCell :: (Int, Int) -> Cell -> Cell
    stepCell pos = \case
      Empty | occupied pos == 0 -> Occupied
      Occupied | occupied pos >= 4 -> Empty
      other -> other

sightNeighbors :: Grid -> (Int, Int) -> [Cell]
sightNeighbors (Grid w h mp) pos = directions |> map findSeat |> catMaybes
  where
    pluckInteresting :: Cell -> Maybe Cell
    pluckInteresting = \case
      Floor -> Nothing
      x -> Just x

    inBounds (x, y) = 0 <= x && x < w && 0 <= y && y < h

    findSeat :: (Int, Int) -> Maybe Cell
    findSeat (dx, dy) =
      iterate (\(x, y) -> (x + dx, y + dy)) pos |> drop 1 |> takeWhile inBounds |> map ((`Map.lookup` mp) >=> pluckInteresting) |> asum

step2 :: Grid -> Grid
step2 grid@(Grid w h mp) = Grid w h (Map.mapWithKey stepCell mp)
  where
    count target = filter (== target) >>> length

    occupied pos = count Occupied (sightNeighbors grid pos)

    stepCell :: (Int, Int) -> Cell -> Cell
    stepCell pos = \case
      Empty | occupied pos == 0 -> Occupied
      Occupied | occupied pos >= 5 -> Empty
      other -> other

countCellType :: Cell -> Grid -> Int
countCellType cell (Grid _ _ mp) = Map.toList mp |> filter ((== cell) . snd) |> length

solve :: (Grid -> Grid) -> Grid -> Maybe Int
solve theStep grid =
  let states = iterate theStep grid
   in states |> zip (drop 1 states) |> find (uncurry (==)) |> fmap (snd >>> countCellType Occupied)

type Input = Grid

readCell :: Char -> Maybe Cell
readCell '.' = Just Floor
readCell 'L' = Just Empty
readCell '#' = Just Occupied
readCell _ = Nothing

readInput :: Text -> Maybe Input
readInput txt = do
  let theLines = lines txt |> map toString
      theHeight = length theLines
  theWidth <- viaNonEmpty (head >>> length) theLines
  let positioned = zip [0 ..] theLines >>= \(y, line) -> zip [0 ..] line |> map (\(x, c) -> ((x, y), c))
  grid <- forM positioned (\(pos, cell) -> (pos,) <$> readCell cell)
  Just (Grid theWidth theHeight (Map.fromList grid))

type Output1 = Maybe Int

solve1 :: Input -> Output1
solve1 = solve step

type Output2 = Maybe Int

solve2 :: Input -> Output2
solve2 = solve step2

theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-11.txt" [] [] [] [] (A.ProblemInfo "Seating System" 2020 11)
