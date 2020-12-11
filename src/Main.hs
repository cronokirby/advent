{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Advent
import Relude
import System.Environment (getArgs)
import qualified Y2018.D1
import qualified Y2019.D1
import qualified Y2019.D2
import qualified Y2019.D3
import qualified Y2019.D4
import qualified Y2019.D5
import qualified Y2019.D6
import qualified Y2020.D1
import qualified Y2020.D2
import qualified Y2020.D3
import qualified Y2020.D4
import qualified Y2020.D5
import qualified Y2020.D6
import qualified Y2020.D7
import qualified Y2020.D8
import qualified Y2020.D9
import qualified Y2020.D10
import qualified Y2020.D11

problems :: [Problem]
problems =
  [ Y2018.D1.problem,
    Y2019.D1.problem,
    Y2019.D2.problem,
    Y2019.D3.problem,
    Y2019.D4.problem,
    Y2019.D5.problem,
    Y2019.D6.problem,
    Y2020.D1.problem,
    Y2020.D2.problem,
    Y2020.D3.problem,
    Y2020.D4.problem,
    Y2020.D5.problem,
    Y2020.D6.problem,
    Y2020.D7.problem,
    Y2020.D8.problem,
    Y2020.D9.problem,
    Y2020.D10.problem,
    Y2020.D11.problem
  ]

runTestFiles ::
  (Show i, Show a) => Solution i a b -> [TestFile] -> IO [TestResult]
runTestFiles solution@Solution {..} testFiles =
  forM (zip [0 ..] testFiles) $ \(index, TestFile {..}) -> do
    inputTxt <- readFileText input
    let inputData = parse inputTxt
    outputData <- readFileText output
    case inputData of
      Nothing ->
        error
          ( "File: "
              <> toText input
              <> " \n"
              <> "Failed to read input:\n"
              <> inputTxt
          )
      Just i ->
        let cse = TestCase i outputData
         in return (runTestCase (textSolution solution) index cse)

main :: IO ()
main = do
  args <- getArgs
  let mb = case args of
        [a, b] -> (,) <$> readMaybe a <*> readMaybe b
        _ -> Nothing
  let probs = case mb of
        Nothing -> problems
        Just (y, d) ->
          filter
            ( \Problem {..} ->
                let ProblemInfo {..} = info in year == y && day == d
            )
            problems
  forM_ probs run
  where
    run p@Problem {..} = do
      let ProblemInfo {..} = info
      putTextLn
        ( mconcat
            [ "\x1b[4m",
              show year,
              "年",
              show day,
              "日 - ",
              name,
              ":",
              "\x1b[0m"
            ]
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
        Nothing -> putTextLn ("Failed to parse prompt:\n" <> prompt)
        Just parsed -> do
          putTextLn "Solution A:"
          putTextLn ("\x1b[36m" <> presentA (solveA parsed) <> "\x1b[0m")
          putTextLn "Solution B:"
          putTextLn ("\x1b[36m" <> presentB (solveB parsed) <> "\x1b[0m")
    printResults msg [] = putTextLn ("\x1b[32m" <> msg <> " Ok!" <> "\x1b[0m")
    printResults msg rs = do
      putTextLn ("\x1b[31m" <> msg)
      forM_ rs (putTextLn . prettyTestResult)
      putText "\x1b[0m"
