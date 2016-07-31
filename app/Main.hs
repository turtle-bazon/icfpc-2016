module Main where

import TrafficLight
import SolutionDP

main :: IO ()
main = do
  putStrLn "Configuration:"
  mapM_ putStrLn $ map show sampleInputErrB
  putStrLn ""
  ppSolution sampleInputErrB
