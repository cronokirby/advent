{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Advent
    ( Solution(..)
    , TestFile(..)
    , TestCase(..)
    , Outcome(..)
    , TestResult(..)
    , ProblemInfo(..)
    , Problem(..)
    , failed
    , prettyTestResult
    , runTestCase
    , runTestCases
    , textSolution
    )
where

import           Relude


data Solution i o = Solution
    { parse :: Text -> Maybe i
    , present :: o -> Text
    , solve :: i -> o
    }

textSolution :: Solution i o -> Solution i Text
textSolution Solution {..} = Solution parse id (present . solve)

data TestFile = TestFile
    { input :: FilePath
    , output :: FilePath
    }
    deriving (Eq, Show)

data TestCase i o = TestCase
    { input :: i
    , output :: o
    }
    deriving (Eq, Show)

data Outcome = Success | Failed deriving (Eq, Show)

data TestResult = forall i o. (Show i, Show o) => TestResult
    { index :: Int
    , input :: i
    , actual :: o
    , expected :: o
    , outcome :: Outcome
    }

failed :: TestResult -> Bool
failed TestResult {..} = outcome == Failed

runTestCase
    :: (Show i, Show o, Eq o)
    => Solution i o
    -> Int
    -> TestCase i o
    -> TestResult
runTestCase Solution {..} index TestCase {..} =
    let expected = output
        actual   = solve input
        outcome  = if expected == actual then Success else Failed
    in  TestResult { .. }

prettyTestResult :: TestResult -> Text
prettyTestResult TestResult {..} =
    let res = case outcome of
            Success -> "Success"
            Failed  -> mconcat
                [ "For input "
                , show input
                , ", expected "
                , show expected
                , " but found "
                , show actual
                ]
    in  "#" <> show index <> ": " <> res


data ProblemInfo = ProblemInfo
    { name :: Text
    , year :: Int
    , day :: Int
    }
    deriving (Eq, Show)

data Problem = forall i o. (Show i, Eq o, Show o) => Problem
    { solution :: Solution i o
    , mainFile :: FilePath
    , testFiles :: [TestFile]
    , testCases :: [TestCase i o]
    , info :: ProblemInfo
    }

runTestCases :: Problem -> [TestResult]
runTestCases Problem {..} =
    testCases & zip [0 ..] & map (uncurry (runTestCase solution))
