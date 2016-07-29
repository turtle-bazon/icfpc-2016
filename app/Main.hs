module Main where

import Developer
import ContribSwizard
import ContribSectoid
import ContribTurtle
import ContribFedor

header :: String
header =
	"Team Skobochka Members"

developers = [swizard, sectoid, turtle, fedor]

developerLine :: Developer -> String
developerLine developer = "* " ++ fullName developer ++ " (" ++ name developer ++ ");"

main :: IO ()
main = do
    mapM_ putStrLn $ header : map developerLine developers

