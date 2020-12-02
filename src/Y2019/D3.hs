{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Y2019.D3
    ( problem
    )
where

import           Relude
import qualified Advent                        as A

import           Data.Foldable                  ( minimum )
import qualified Data.HashSet                  as Set
import qualified Data.Text                     as T

data Move = R Int | U Int | D Int | L Int deriving (Eq, Show)

data Input = Input [Move] [Move] deriving (Eq, Show)

readInput :: Text -> Maybe Input
readInput input = do
    ls <- mapM readMoves (lines input)
    case ls of
        [a, b] -> Just (Input a b)
        _      -> Nothing
  where
    readMoves :: Text -> Maybe [Move]
    readMoves = mapM readMove . T.splitOn ","
    readMove :: Text -> Maybe Move
    readMove input = do
        (c, cs) <- T.uncons input
        int     <- rightToMaybe $ readEither (toString cs)
        case c of
            'R' -> Just (R int)
            'U' -> Just (U int)
            'D' -> Just (D int)
            'L' -> Just (L int)

manhattanD :: (Int, Int) -> Int
manhattanD (x, y) = abs x + abs y

points :: [Move] -> Set.HashSet (Int, Int)
points = Set.delete (0, 0) . fst . foldl' go (Set.empty, (0, 0))
  where
    between a b = case compare a b of
        GT -> [b .. a]
        EQ -> [a]
        LT -> [a .. b]
    go (set, (x, y)) move =
        let (nX, nY) = case move of
                R i -> (x + i, y)
                L i -> (x - i, y)
                D i -> (x, y - i)
                U i -> (x, y + i)
            toAdd = [ (x', y') | x' <- between x nX, y' <- between y nY ]
        in  (Set.union set (Set.fromList toAdd), (nX, nY))


intersections :: [Move] -> [Move] -> Set.HashSet (Int, Int)
intersections m1 m2 = Set.intersection (points m1) (points m2)

solve1 :: Input -> Int
solve1 (Input m1 m2) =
    minimum . map manhattanD $ Set.toList (intersections m1 m2)

distanceToPoint :: (Int, Int) -> [Move] -> Int
distanceToPoint = go (0, 0) 0
  where
    go pos@(x, y) steps target moves
      | target == pos = steps
      | otherwise = case moves of
        [] -> error "failed to reach found intersection"
        (L 0:ms) -> go pos steps target ms
        (R 0:ms) -> go pos steps target ms
        (U 0:ms) -> go pos steps target ms
        (D 0:ms) -> go pos steps target ms
        (L i:ms) -> go (x - 1, y) (steps + 1) target (L (i - 1):ms)
        (R i:ms) -> go (x + 1, y) (steps + 1) target (R (i - 1):ms)
        (U i:ms) -> go (x, y + 1) (steps + 1) target (U (i - 1):ms)
        (D i:ms) -> go (x, y - 1) (steps + 1) target (D (i - 1):ms)

solve2 :: Input -> Int
solve2 (Input m1 m2) =
    let inters = intersections m1 m2
        distances = map (\p -> sum $ map (distanceToPoint p) [m1, m2]) $ Set.toList inters
    in minimum distances

theSolution :: A.Solution Input Int Int
theSolution = A.Solution readInput show show solve1 solve2

testsA :: [A.TestCase Input Int]
testsA =
    [ A.TestCase (Input [R 8, U 5, L 5, D 3] [U 7, R 6, D 4, L 4]) 6
    ]

testsB :: [A.TestCase Input Int]
testsB =
    [ A.TestCase (Input [R 8, U 5, L 5, D 3] [U 7, R 6, D 4, L 4]) 30 ]

problem :: A.Problem
problem = A.Problem theSolution
                    "data/prompt-2019-3.txt"
                    []
                    []
                    testsA
                    testsB
                    (A.ProblemInfo "Crossed Wires" 2019 3)
