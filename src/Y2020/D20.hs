{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Y2020.D20 where

import qualified Advent as A
import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Ourlude

data Grid = Grid Int (Set.Set (Int, Int)) deriving (Eq, Show)

data Piece = Piece {pieceID :: Int, pieceGrid :: Grid} deriving (Eq, Show)

type Input = [Piece]

readInput :: Text -> Maybe Input
readInput = T.splitOn "\n\n" >>> traverse get
  where
    get :: Text -> Maybe Piece
    get txt = do
      (firstPart, secondPart) <- uncons (lines txt)
      pieceID <- firstPart |> toString |> filter isDigit |> readMaybe
      let size = length secondPart
          set =
            Set.fromList <| do
              (y, line) <- zip [0 ..] secondPart
              (x, c) <- zip [0 ..] (toString line)
              guard (c == '#')
              return (x, y)
      return (Piece pieceID (Grid size set))

rotate :: Piece -> Piece
rotate (Piece i grid) = Piece i (rotateGrid grid)
  where
    rotateGrid (Grid size items) = Grid size (Set.map (\(x, y) -> (size - 1 - y, x)) items)

flipPiece :: Piece -> Piece
flipPiece (Piece i grid) = Piece i (flipGrid grid)
  where
    flipGrid (Grid size items) = Grid size (Set.map (\(x, y) -> (size - 1 - x, y)) items)

-- These are all elements of the dihedral group D8
actions :: [Piece -> Piece]
actions = [maybeFlip >>> powN rotate n | n <- [0 .. 3], maybeFlip <- [id, flipPiece]]
  where
    powN f n = foldr (>>>) id (f <$ [1 .. n])

arrangements :: Piece -> [Piece]
arrangements piece = map (piece |>) actions

data Edge = BottomEdge | TopEdge | LeftEdge | RightEdge deriving (Show)

oppositeEdge :: Edge -> Edge
oppositeEdge = \case
  BottomEdge -> TopEdge
  TopEdge -> BottomEdge
  LeftEdge -> RightEdge
  RightEdge -> LeftEdge

pieceEdge :: Edge -> Piece -> Set.Set Int
pieceEdge edge (Piece _ (Grid size set)) =
  Set.fromList <| case edge of
    BottomEdge -> [x | x <- [0 .. lastEl], Set.member (x, lastEl) set]
    TopEdge -> [x | x <- [0 .. lastEl], Set.member (x, 0) set]
    LeftEdge -> [y | y <- [0 .. lastEl], Set.member (0, y) set]
    RightEdge -> [y | y <- [0 .. lastEl], Set.member (lastEl, y) set]
  where
    lastEl = size - 1

pieceMatch :: Edge -> Piece -> Piece -> Bool
pieceMatch edge p1 p2 = pieceEdge edge p1 == pieceEdge (oppositeEdge edge) p2

type Placement = Map.Map (Int, Int) Piece

data Context = Context
  { placeSize :: Int,
    placed :: Placement,
    pieces :: [Piece],
    currentPos :: (Int, Int)
  }

type SolverM a = Reader Context a

canPlace :: Piece -> SolverM Bool
canPlace piece = do
  Context {..} <- ask
  let placedIDs = placed |> Map.elems |> map pieceID
      notAlreadyPlaced = pieceID piece `notElem` placedIDs
      (currX, currY) = currentPos
      matchesEdge edge pos = case Map.lookup pos placed of
        Nothing -> True
        Just piece2 -> pieceMatch edge piece piece2
  return (and [notAlreadyPlaced, matchesEdge TopEdge (currX, currY - 1), matchesEdge LeftEdge (currX - 1, currY)])

withPlacement :: Piece -> SolverM a -> SolverM a
withPlacement piece =
  local <| \ctx@Context {..} ->
    let (x, y) = currentPos
     in ctx
          { currentPos = ((x + 1) `mod` placeSize, y + (x + 1) `div` placeSize),
            placed = Map.insert currentPos piece placed
          }

makeContext :: [Piece] -> Context
makeContext pieces = Context placeSize mempty pieces (0, 0)
  where
    placeSize = length pieces |> fromIntegral |> sqrt |> floor

solve :: SolverM (Maybe Placement)
solve = do
  Context {..} <- ask
  if Map.size placed == length pieces
    then return (Just placed)
    else do
      placeable <- filterM canPlace (pieces >>= arrangements)
      let go [] = return Nothing
          go (x : xs) =
            withPlacement x solve >>= \case
              Nothing -> go xs
              Just solution -> return (Just solution)
      go placeable

findPlacement :: Context -> Maybe Placement
findPlacement = runReader solve

getCorners :: Context -> Placement -> [Piece]
getCorners Context {..} mp =
  map (`Map.lookup` mp) [(x, y) | x <- [0, placeSize - 1], y <- [0, placeSize - 1]] |> catMaybes

type Output1 = Maybe Int

solve1 :: Input -> Output1
solve1 pieces = do
  placement <- findPlacement ctx
  return (placement |> getCorners ctx |> map pieceID |> product)
  where
    ctx = makeContext pieces

type Output2 = ()

solve2 :: Input -> Output2
solve2 _ = ()

theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-20.txt" [] [] [] [] (A.ProblemInfo "TODO" 2020 20)
