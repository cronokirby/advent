{-# LANGUAGE LambdaCase #-}

module Y2020.D19 where

import qualified Advent as A
import Data.Char (isDigit)
import Data.Map.Strict ((!))
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import Ourlude
import qualified Text.ParserCombinators.ReadP as P

data Rule
  = After Int Int
  | Char Char
  | Or [Int] [Int]
  | Indirect Int
  deriving (Eq, Show)

type Input = (Map.Map Int Rule, [Text])

readInput :: Text -> Maybe Input
readInput txt = do
  (rulePart, linesPart) <- case T.splitOn "\n\n" txt of
    [a, b] -> Just (a, b)
    _ -> Nothing
  rules <- getRules rulePart
  return (rules, lines linesPart)
  where
    getRules :: Text -> Maybe (Map.Map Int Rule)
    getRules = lines >>> traverse getRule >>> fmap Map.fromList

    getRule :: Text -> Maybe (Int, Rule)
    getRule = toString >>> P.readP_to_S indexedRule >>> viaNonEmpty (head >>> fst)

indexedRule :: P.ReadP (Int, Rule)
indexedRule = do
  index <- int
  void (P.string ": ")
  rule' <- rule
  P.eof
  return (index, rule')

rule :: P.ReadP Rule
rule = (uncurry After <$> afterRule) <|> charRule <|> orRule <|> indirectRule
  where
    afterRule = (,) <$> int <*> (P.skipSpaces *> int)
    indirectRule = Indirect <$> int
    charRule = Char <$> (P.char '"' *> P.satisfy (const True) <* P.char '"')
    orRule = do
      let ints = P.sepBy1 int P.skipSpaces
      r1 <- ints
      void (P.string " | ")
      r2 <- ints
      return (Or r1 r2)

int :: P.ReadP Int
int = do
  Just x <- readMaybe <$> P.munch1 isDigit
  return x

type Output1 = Int

makeParser :: Map.Map Int Rule -> P.ReadP ()
makeParser ruleMap = do
  parserMap ! 0
  P.eof
  where
    parserMap :: Map.Map Int (P.ReadP ())
    parserMap = Map.map getP ruleMap
      where
        getP :: Rule -> P.ReadP ()
        getP = \case
          Char c -> void (P.char c)
          Indirect j -> parserMap ! j
          After i1 i2 -> parserMap ! i1 *> parserMap ! i2
          Or a b -> void (traverse (parserMap !) a <|> traverse (parserMap !) b)

parses :: P.ReadP a -> Text -> Bool
parses p = toString >>> P.readP_to_S p >>> null >>> not

solve1 :: Input -> Output1
solve1 (rules, messages) = messages |> filter (parses (makeParser rules)) |> length

type Output2 = Int

solve2 :: Input -> Output2
solve2 (rules, messages) = messages |> filter (parses (makeParser rules')) |> length
  where
    newRules =
      Map.fromList
        [ (0, Or [42] [42, 8]),
          (11, Or [42, 31] [42, 11, 31])
        ]
    rules' = newRules <> rules

theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/input-2020-19-A-0.txt" [] [] [] [] (A.ProblemInfo "TODO" 2020 19)
