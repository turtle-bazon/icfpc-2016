module Main where

import BoundingBox

main :: IO ()
main = do
  contents <- getContents
  printSolution $ bbox $ parseFirstPoly contents
