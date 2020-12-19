{-# LANGUAGE LambdaCase #-}

module Y2020.D19 where

import qualified Advent as A
import Data.Char (isDigit)
import qualified Data.Map.Lazy as Map
import Data.Map.Strict ((!))
import qualified Data.Text as T
import Ourlude
import qualified Text.ParserCombinators.ReadP as P

data Rule
  = Chain [Int]
  | Char Char
  | Or Rule Rule
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
rule = chainRule <|> charRule <|> orRule
  where
    chainRule = Chain <$> P.sepBy1 int P.skipSpaces
    charRule = Char <$> (P.char '"' *> P.satisfy (const True) <* P.char '"')
    orRule = do
      r1 <- chainRule
      void (P.string " | ")
      r2 <- chainRule
      return (Or r1 r2)

int :: P.ReadP Int
int = do
  Just x <- readMaybe <$> P.munch1 isDigit
  return x

type Output1 = Int

makeParser :: Map.Map Int Rule -> P.ReadP ()
makeParser ruleMap = do
  let parserMap = makeParserMap ruleMap
  parserMap ! 0
  P.eof
  where
    makeParserMap :: Map.Map Int Rule -> Map.Map Int (P.ReadP ())
    makeParserMap ruleMap = parserMap
      where
        parserMap :: Map.Map Int (P.ReadP ())
        parserMap = Map.map getP ruleMap
          where
            getP :: Rule -> P.ReadP ()
            getP = \case
              Char c -> void (P.char c)
              Chain is -> traverse_ (parserMap !) is
              Or a b -> getP a <|> getP b

parses :: P.ReadP a -> Text -> Bool
parses p = toString >>> P.readP_to_S p >>> null >>> not

solve1 :: Input -> Output1
solve1 (rules, messages) = messages |> filter (parses (makeParser rules)) |> length

type Output2 = Int

solve2 :: Input -> Output2
solve2 (rules, messages) = messages |> filter (parses (makeParser rules')) |> length
  where
    newRules =
      [ (8, Or (Chain [42]) (Chain [42, 8])),
        (11, Or (Chain [42, 31]) (Chain [42, 11, 31]))
      ]
    rules' = Map.fromList newRules <> rules

theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-19.txt" [] [] [] [] (A.ProblemInfo "TODO" 2020 19)
