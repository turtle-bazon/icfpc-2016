module Main where

import System.Environment
import Common
import Figures
import SolversRun

main :: IO ()
main = do
  args <- getArgs
  processArgs args
      where
        processArgs (problemFile : solutionFile : _) = do
          problem <- loadProblem $ problemFile
          solution <- loadSolution $ solutionFile
          sim <- score (silhouette problem) solution
          putStrLn $ show sim
        processArgs _ = do
          processName <- getProgName
          putStrLn $ "Usage: " ++ processName ++ " <problem file> <solution file>"
