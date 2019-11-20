{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Relude

import           Advent

problems :: [Problem]
problems =
    [ Problem
          s
          "data/prompt-2019-0.txt"
          [TestFile "data/input-2019-0-0.txt" "data/output-2019-0-0.txt"]
          [TestCase 0 3]
          (ProblemInfo "Example" 2019 0)
    ]
  where
    s :: Solution Int Int
    s = Solution (rightToMaybe . readEither) show (+ 3)

runTestFiles :: Problem -> IO [TestResult]
runTestFiles Problem {..} = do
    let Solution {..} = solution
    forM (zip [0 ..] testFiles) $ \(index, TestFile {..}) -> do
        inputTxt <- readFileText input
        let inputData = parse inputTxt
        outputData <- readFileText output
        case inputData of
            Nothing -> error
                (  "File: "
                <> toText input
                <> " \n"
                <> "Failed to read input:\n"
                <> inputTxt
                )
            Just i ->
                let
                    cse        = TestCase i outputData
                in  return (runTestCase (textSolution solution) index cse)

main :: IO ()
main = forM_ problems $ \p@Problem {..} -> do
    let ProblemInfo {..} = info
    putTextLn (mconcat [show year <> "年" <> show day <> "日 - " <> name <> ":"])
    putText "Test Files:"
    fileResults <- filter failed <$> runTestFiles p
    printResults fileResults
    putText "Test Cases:"
    let caseResults = filter failed (runTestCases p)
    printResults caseResults
  where
    printResults [] = putTextLn " Ok!"
    printResults rs = do
        putTextLn ""
        forM_ rs (putTextLn . prettyTestResult)
