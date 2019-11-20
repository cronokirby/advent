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
                let cse = TestCase i outputData
                in  return (runTestCase (textSolution solution) index cse)

main :: IO ()
main = forM_ problems $ \p@Problem {..} -> do
    let ProblemInfo {..} = info
    putTextLn (mconcat ["\x1b[4m", show year, "年", show day, "日 - ", name, ":", "\x1b[0m"])
    fileResults <- filter failed <$> runTestFiles p
    printResults "Test Files:" fileResults
    let caseResults = filter failed (runTestCases p)
    printResults "Test Cases:" caseResults
    putTextLn "Solution:"
    prompt <- readFileText promptFile
    let Solution{..} = solution
    case parse prompt of
        Nothing -> putTextLn ("Failed to parse prompt:\n" <> prompt)
        Just parsed -> putTextLn ("\x1b[36m" <> show (solve parsed) <> "\x1b[0m")
  where
    printResults msg [] = putTextLn ("\x1b[32m" <> msg <> " Ok!" <> "\x1b[0m")
    printResults msg rs = do
        putTextLn ("\x1b[31m" <> msg)
        forM_ rs (putTextLn . prettyTestResult)
        putText "\x1b[0m"
