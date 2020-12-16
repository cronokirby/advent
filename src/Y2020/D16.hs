module Y2020.D16 (problem) where

import qualified Advent as A
import Data.Char (isDigit)
import Data.List (isPrefixOf, (!!))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Ourlude
import Text.ParserCombinators.ReadP as P

data Rule = Rule String (Int, Int) (Int, Int) deriving (Eq, Show)

data Ticket = Ticket [Int] deriving (Eq, Show)

data Input = Input [Rule] Ticket [Ticket] deriving (Eq, Show)

readInput :: Text -> Maybe Input
readInput = toString >>> P.readP_to_S input >>> viaNonEmpty (head >>> fst)
  where
    input :: P.ReadP Input
    input = do
      rules <- P.sepBy1 rule (P.char '\n')
      void (P.string "\n\nyour ticket:\n")
      myTicket <- ticket
      void (P.string "\n\nnearby tickets:\n")
      otherTickets <- P.sepBy1 ticket (P.char '\n')
      void (P.char '\n')
      void (P.eof)
      return (Input rules myTicket otherTickets)

    int :: P.ReadP Int
    int = do
      Just x <- readMaybe <$> P.many1 (P.satisfy isDigit)
      return x

    range :: P.ReadP (Int, Int)
    range = do
      start <- int
      void (P.char '-')
      end <- int
      return (start, end)

    label :: P.ReadP String
    label = P.munch1 (/= ':') <* P.char ':'

    rule :: P.ReadP Rule
    rule = do
      label' <- label
      void (P.char ' ')
      range1 <- range
      void (P.string " or ")
      range2 <- range
      return (Rule label' range1 range2)

    ticket :: P.ReadP Ticket
    ticket = Ticket <$> P.sepBy1 int (P.char ',')

type Output1 = Int

inRange :: Int -> (Int, Int) -> Bool
inRange x (a, b) = a <= x && x <= b

satisfies :: Int -> Rule -> Bool
satisfies x (Rule _ r1 r2) = inRange x r1 || inRange x r2

solve1 :: Input -> Output1
solve1 (Input rules _ tickets) =
  tickets |> foldMap sumInvalid |> getSum
  where
    sumInvalid :: Ticket -> Sum Int
    sumInvalid (Ticket fields) = fields |> filter (not <<< valid) |> foldMap Sum

    valid :: Int -> Bool
    valid x = any (\(Rule _ r1 r2) -> inRange x r1 || inRange x r2) rules

type Output2 = Maybe Int

type Index = Int

type Assignment = Map.Map String Index

simpleValidate :: [Rule] -> [Ticket] -> [Ticket]
simpleValidate rules = filter maybeValidTicket
  where
    maybeValidTicket :: Ticket -> Bool
    maybeValidTicket (Ticket fields) = any maybeValid fields

    maybeValid :: Int -> Bool
    maybeValid x = any (satisfies x) rules

ticketCol :: Index -> Ticket -> Int
ticketCol i (Ticket fields) = fields !! i

solve2 :: Input -> Output2
solve2 (Input rules myTicket otherTickets) = do
  assignment <- search 0 []
  let columns = assignment |> filter (snd >>> isPrefixOf "deparature") |> map fst
  return (columns |> map (`ticketCol` myTicket) |> product)
  where
    tickets :: [Ticket]
    tickets = simpleValidate rules otherTickets

    labels :: [String]
    labels = map (\(Rule s _ _) -> s) rules

    ruleMap :: Map.Map String Rule
    ruleMap = rules |> map (\rule@(Rule s _ _) -> (s, rule)) |> Map.fromList

    ticketLength :: Int
    ticketLength =
       let (Ticket fields) = myTicket
       in length fields

    indices :: [Index]
    indices = [0..ticketLength - 1]

    choices :: Map.Map Index (Set.Set String)
    choices = Map.fromList [(i, Set.fromList (filter (`possible` i) labels)) | i <- indices]
      where
        possible :: String -> Int -> Bool
        possible label col =
          let Just rule = Map.lookup label ruleMap
          in tickets |> map (ticketCol col) |> all (`satisfies` rule)

    search :: Index -> [(Int, String)] -> Maybe [(Int, String)]
    search i assignment | i == ticketLength = return assignment
    search i assignment =
      let allChoices = Map.findWithDefault mempty i choices
          notUsed = Set.difference allChoices (Set.fromList (map snd assignment))
          choose s = search (i + 1) ((i, s) : assignment)
      in asumMap choose notUsed

theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-16.txt" [] [] [] [] (A.ProblemInfo "Ticket Translation" 2020 16)
