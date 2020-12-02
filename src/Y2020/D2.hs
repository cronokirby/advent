module Y2020.D2 (problem) where

import qualified Advent as A
import Data.Char (isDigit)
import Ourlude
import Text.ParserCombinators.ReadP as P

data Rule = Rule Int Int Char deriving (Eq, Show)

data Password = Password Rule String deriving (Eq, Show)

type Input = [Password]

readInput :: Text -> Maybe Input
readInput = lines >>> traverse get
  where
    get = toString >>> P.readP_to_S password >>> viaNonEmpty (head >>> fst)

    password :: P.ReadP Password
    password = do
      rule' <- rule
      P.char ':'
      P.skipSpaces
      rest <- P.munch1 (const True)
      return (Password rule' rest)

    int :: P.ReadP Int
    int = do
      Just x <- readMaybe <$> P.munch1 isDigit
      return x

    rule :: P.ReadP Rule
    rule = do
      i1 <- int
      P.char '-'
      i2 <- int
      P.skipSpaces
      c <- P.satisfy (const True)
      return (Rule i1 i2 c)

solve1 :: Input -> Int
solve1 = filter isGood >>> length
  where
    inRange lo hi x = lo <= x && x <= hi

    isGood :: Password -> Bool
    isGood (Password (Rule lo hi c) s) =
      filter (== c) s |> length |> inRange lo hi

solve2 :: Input -> Int
solve2 = filter isGood >>> length
  where
    isGood :: Password -> Bool
    isGood (Password (Rule lo hi c) s) =
        equalAt lo /= equalAt hi
      where
        equalAt i = (s !!? (i - 1)) == Just c

theSolution :: A.Solution Input Int Int
theSolution = A.Solution readInput show show solve1 solve2

testFilesA :: [A.TestFile]
testFilesA =
  [A.TestFile "data/input-2020-2-A-0.txt" "data/output-2020-2-A-0.txt"]

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-2.txt" testFilesA [] [] [] (A.ProblemInfo "Password Philosophy" 2020 2)
