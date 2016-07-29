module Main where

import ContribSwizard
import ContribSectoid

header :: IO ()
header =
    putStrLn "Team Skobochka Members"

main :: IO ()
main =
    sequence_ [ header
              , printSwizard
              , printSectoid
              ]
