module Main where

import BoundingBox
import Show

main :: IO ()
main = do
  contents <- getContents
  --putStr $ showSolution $ makeRectangleSolution $ parseFirstPoly contents
  putStr $ showSolution $ makeRectangleSolution $ parseAll contents
