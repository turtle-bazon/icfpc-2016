module Main where

import TrafficLight
import SolutionDP

main :: IO ()
main = do
  putStrLn "Configuration:"
  mapM_ putStrLn $ map show sampleInput
  putStrLn ""
  ppSolution sampleInputErrA
