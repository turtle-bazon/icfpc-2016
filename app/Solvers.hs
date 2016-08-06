module Main where

import System.Environment
import SolversRun

main :: IO ()
main = do
  args <- getArgs
  processArgs args
      where
        processArgs (problemsDir : outputDir : _) = run problemsDir outputDir
        processArgs _ = do
          processName <- getProgName
          putStrLn $ "Usage: " ++ processName ++ " <problems input directory> <solutions output directory>"
