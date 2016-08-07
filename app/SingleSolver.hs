module Main where

import System.Environment
import SolversRun

main :: IO ()
main = do
  args <- getArgs
  processArgs args
      where
        processArgs (solverName : problem : _) = runSingle solverName problem
        processArgs _ = do
          processName <- getProgName
          putStrLn $ "Usage: " ++ processName ++ " <solver name> <problem>"
