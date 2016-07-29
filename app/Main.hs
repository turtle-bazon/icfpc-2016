module Main where

import ContribSwizard

header :: IO ()
header =
    putStrLn "Team Skobochka Members"
    
main :: IO ()
main =
    sequence_ [ header
              , printSwizard
              ]
