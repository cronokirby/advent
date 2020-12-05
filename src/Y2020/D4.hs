{-# LANGUAGE TypeApplications #-}

module Y2020.D4 (problem) where

import qualified Advent as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Ourlude

type Input = [HM.HashMap Text Text]

readInput :: Text -> Maybe Input
readInput = T.splitOn "\n\n" >>> traverse parseMap
  where
    parseMap :: Text -> Maybe (HM.HashMap Text Text)
    parseMap = T.strip >>> T.words >>> traverse parseEntry >>> fmap HM.fromList

    parseEntry :: Text -> Maybe (Text, Text)
    parseEntry txt = case T.splitOn ":" txt of
      [k, v] -> Just (k, v)
      _ -> Nothing

type Output1 = Int

solve1 :: Input -> Int
solve1 = filter (\passport -> all (`HM.member` passport) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]) >>> length

type Output2 = Int

data Height = Inches Int | Centimeters Int

solve2 :: Input -> Output2
solve2 = filter isValid >>> length
  where
    readInt :: Text -> Maybe Int
    readInt = toString >>> readMaybe

    readHeight :: Text -> Maybe Height
    readHeight txt =
      (T.stripSuffix "cm" txt >>= readInt |> fmap Centimeters)
        <|> (T.stripSuffix "in" txt >>= readInt |> fmap Inches)

    inRange :: (Int, Int) -> Int -> Bool
    inRange (lo, hi) x = lo <= x && x <= hi

    nums :: String
    nums = "0123456789"

    alphaNum :: String
    alphaNum = nums ++ "abcdef"

    eyeColors :: [Text]
    eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

    isValid :: HM.HashMap Text Text -> Bool
    isValid mp =
      isJust <| do
        byr <- HM.lookup "byr" mp >>= readInt
        guard (inRange (1920, 2002) byr)
        iyr <- HM.lookup "iyr" mp >>= readInt
        guard (inRange (2010, 2020) iyr)
        eyr <- HM.lookup "eyr" mp >>= readInt
        guard (inRange (2020, 2030) eyr)
        hgt <- HM.lookup "hgt" mp >>= readHeight
        let validHeight = case hgt of
              Inches i -> inRange (59, 76) i
              Centimeters cm -> inRange (150, 193) cm
        guard validHeight
        hcl <- HM.lookup "hcl" mp >>= T.stripPrefix "#"
        guard (T.all (`elem` alphaNum) hcl && T.length hcl == 6)
        ecl <- HM.lookup "ecl" mp
        guard (ecl `elem` eyeColors)
        pid <- HM.lookup "pid" mp
        guard (T.all (`elem` nums) pid && T.length pid == 9)
        return ()

theSolution :: A.Solution Input Output1 Output2
theSolution = A.Solution readInput show show solve1 solve2

testFilesB :: [A.TestFile]
testFilesB =
  [ A.TestFile "data/input-2020-4-B-0.txt" "data/output-2020-4-B-0.txt",
    A.TestFile "data/input-2020-4-B-1.txt" "data/output-2020-4-B-1.txt"
  ]

problem :: A.Problem
problem = A.Problem theSolution "data/prompt-2020-4.txt" [] testFilesB [] [] (A.ProblemInfo "Passport Processing" 2020 4)
