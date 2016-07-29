module Main where

import ContribSwizard
import ContribSectoid
import ContribTurtle
import ContribFedor

header :: String
header =
	"Team Skobochka Members"

main :: IO [()]
main = do
    mapM putStrLn [ header 
                  , printSwizard
                  , printSectoid
                  , printTurtle
                  , printFedor
                  ]

