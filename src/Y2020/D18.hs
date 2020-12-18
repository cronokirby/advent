module Y2020.D18 where

import qualified Advent as A
import Data.Char (isDigit, isSpace)
import Ourlude
import qualified Text.ParserCombinators.ReadP as P

type Input = [Text]

readInput :: Text -> Maybe Input
readInput = lines >>> Just

type Output1 = Maybe Int

runReadP :: P.ReadP a -> Text -> Maybe a
runReadP parser =
  toString >>> filter (not <<< isSpace) >>> P.readP_to_S parser >>> viaNonEmpty (head >>> fst)

int :: P.ReadP Int
int = do
  Just x <- readMaybe <$> P.munch1 isDigit
  return x

opsL :: P.ReadP a -> P.ReadP (a -> a -> a) -> P.ReadP a
opsL p sep = liftA2 (foldl' (|>)) p (many (liftA2 (<|) sep p))

mul :: P.ReadP (Int -> Int -> Int)
mul = (*) <$ P.char '*'

add :: P.ReadP (Int -> Int -> Int)
add = (+) <$ P.char '+'

expression1 :: P.ReadP Int
expression1 = opsL atom (mul <|> add)
  where
    atom = int <|> (P.char '(' *> expression1 <* P.char ')')

runArithmetic1 :: Text -> Maybe Int
runArithmetic1 = runReadP (expression1 <* P.eof)

solve1 :: Input -> Output1
solve1 = traverse runArithmetic1 >>> fmap sum

type Output2 = Maybe Int

expression2 :: P.ReadP Int
expression2 = timesExpr
  where
    timesExpr = opsL addExpr mul

    addExpr = opsL atom add

    atom = int <|> (P.char '(' *> expression2 <* P.char ')')

runArithmetic2 :: Text -> Maybe Int
runArithmetic2 = runReadP (expression2 <* P.eof)

solve2 :: Input -> Output2
solve2 = traverse runArithmetic2 >>> fmap sum

theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-18.txt" [] [] [] [] (A.ProblemInfo "TODO" 2020 18)
