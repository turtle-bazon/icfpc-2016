module Main where

import BoundingBox
import Show

main :: IO ()
main = do
  contents <- getContents
  putStr $ showSolution $ makeSolution $ bbox $ parseFirstPoly contents
