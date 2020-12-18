module Y2020.D18 where

import qualified Advent as A
import Data.Char (isDigit, isSpace)
import Ourlude
import qualified Text.ParserCombinators.ReadP as P

type Input = [Text]

readInput :: Text -> Maybe Input
readInput = lines >>> Just

type Output1 = Int

runReadP :: P.ReadP a -> String -> Maybe a
runReadP parser = filter (not <<< isSpace) >>> P.readP_to_S parser >>> viaNonEmpty (head >>> fst)

int :: P.ReadP Int
int = do
  Just x <- readMaybe <$> P.munch1 isDigit
  return x

expression1 :: P.ReadP Int
expression1 = atom <|> binExpr '+' (+) <|> binExpr '*' (*)
  where
    binExpr c f = do
      a <- atom
      void (P.char c)
      b <- expression1
      return (f a b)

    atom = int <|> (P.char ')' *> expression1 <* P.char '(')

runArithmetic1 :: Text -> Maybe Int
runArithmetic1 = toString >>> reverse >>> runReadP (expression1 <* P.eof)

solve1 :: Input -> Output1
solve1 = map (runArithmetic1 >>> fromMaybe 0) >>> sum

type Output2 = Maybe Int

expression2 :: P.ReadP Int
expression2 = timesExpr
  where
    timesExpr = product <$> P.sepBy1 addExpr (P.char '*')

    addExpr = sum <$> P.sepBy1 atom (P.char '+')

    atom = int <|> (P.char '(' *> expression2 <* P.char ')')

runArithmetic2 :: Text -> Maybe Int
runArithmetic2 = toString >>> runReadP (expression2 <* P.eof)

solve2 :: Input -> Output2
solve2 = traverse runArithmetic2 >>> fmap sum

theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-18.txt" [] [] [] [] (A.ProblemInfo "TODO" 2020 18)
