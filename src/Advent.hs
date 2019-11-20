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
    , runTestCasesA
    , runTestCasesB
    , textSolution
    )
where

import           Relude


data Solution i a b = Solution
    { parse :: Text -> Maybe i
    , presentA :: a -> Text
    , presentB :: b -> Text
    , solveA :: i -> a
    , solveB :: i -> b
    }

textSolution :: Solution i a b -> Solution i Text Text
textSolution Solution {..} =
    Solution parse id id (presentA . solveA) (presentB . solveB)

swapSolution :: Solution i a b -> Solution i b a
swapSolution Solution {..} = Solution parse presentB presentA solveB solveA

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
    => Solution i o x
    -> Int
    -> TestCase i o
    -> TestResult
runTestCase Solution {..} index TestCase {..} =
    let expected = output
        actual   = solveA input
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

data Problem = forall i a b. (Show i, Eq a, Show a, Eq b, Show b) => Problem
    { solution :: Solution i a b
    , promptFile :: FilePath
    , testFilesA :: [TestFile]
    , testFilesB :: [TestFile]
    , testCasesA :: [TestCase i a]
    , testCasesB :: [TestCase i b]
    , info :: ProblemInfo
    }

runTestCasesA :: Problem -> [TestResult]
runTestCasesA Problem {..} =
    testCasesA & zip [0 ..] & map (uncurry (runTestCase solution))

runTestCasesB :: Problem -> [TestResult]
runTestCasesB Problem {..} =
    testCasesB & zip [0 ..] & map (uncurry (runTestCase (swapSolution solution)))
