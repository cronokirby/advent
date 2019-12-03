{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Relude

import           Advent

import qualified Y2018.D1
import qualified Y2019.D1
import qualified Y2019.D2
import qualified Y2019.D3

problems :: [Problem]
problems =
    [Y2018.D1.problem, Y2019.D1.problem, Y2019.D2.problem, Y2019.D3.problem]
  where
    s :: Solution Int Int Int
    s = Solution (rightToMaybe . readEither) show show (+ 3) (+ 10)

runTestFiles
    :: (Show i, Show a) => Solution i a b -> [TestFile] -> IO [TestResult]
runTestFiles solution@Solution {..} testFiles =
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
    putTextLn
        (mconcat
            ["\x1b[4m", show year, "年", show day, "日 - ", name, ":", "\x1b[0m"]
        )
    fileResultsA <- filter failed <$> runTestFiles solution testFilesA
    printResults "Test Files A:" fileResultsA
    fileResultsB <-
        filter failed <$> runTestFiles (swapSolution solution) testFilesB
    printResults "Test Files B:" fileResultsB
    let caseResultsA = filter failed (runTestCasesA p)
    printResults "Test Cases A:" caseResultsA
    let caseResultsB = filter failed (runTestCasesB p)
    printResults "Test Cases B:" caseResultsB
    let Solution {..} = solution
    prompt <- readFileText promptFile
    case parse prompt of
        Nothing     -> putTextLn ("Failed to parse prompt:\n" <> prompt)
        Just parsed -> do
            putTextLn "Solution A:"
            putTextLn ("\x1b[36m" <> presentA (solveA parsed) <> "\x1b[0m")
            putTextLn "Solution B:"
            putTextLn ("\x1b[36m" <> presentB (solveB parsed) <> "\x1b[0m")
  where
    printResults msg [] = putTextLn ("\x1b[32m" <> msg <> " Ok!" <> "\x1b[0m")
    printResults msg rs = do
        putTextLn ("\x1b[31m" <> msg)
        forM_ rs (putTextLn . prettyTestResult)
        putText "\x1b[0m"
