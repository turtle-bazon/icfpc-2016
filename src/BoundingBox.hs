module Main where

import Data.Ratio

data Point = Point (Ratio Int) (Ratio Int) deriving (Eq, Show)

px :: Point -> Ratio Int
px (Point x _) = x

py :: Point -> Ratio Int
py (Point _ y) = y

parseRatio :: String -> Ratio Int
parseRatio string =
    numerator % denominator
        where
          numerator = read $ takeWhile ((/=) '/') string
          denominator = parseDenominatorString $ dropWhile ((/=) '/') string
          parseDenominatorString "" = 1
          parseDenominatorString other = read $ tail $ other

parsePoint :: String -> Point
parsePoint line =
    Point x y
        where
          x = parseRatio $ takeWhile ((/=) ',') line
          y = parseRatio $ tail $ dropWhile ((/=) ',') line

parsePoly :: String -> [Point]
parsePoly contents =
    let
        countString : restStrings = drop 1 $ lines contents
        count = read countString
    in
      map parsePoint $ take count $ restStrings

bbox :: [Point] -> (Point, Point)
bbox points =
    (topLeft, bottomRight)
        where
          topLeft = Point minX minY
          bottomRight = Point maxX maxY
          minX = minimum $ map px points
          minY = minimum $ map py points
          maxX = maximum $ map px points
          maxY = maximum $ map py points

showRatio :: Ratio Int -> String
showRatio v =
    case denominator v of
      1 -> show $ numerator v
      d -> (show $ numerator v) ++ "/" ++ (show d)

showPoint :: Point -> String
showPoint (Point x y) =
    (showRatio x) ++ "," ++ (showRatio y)

printSolution :: (Point, Point) -> IO ()
printSolution ((Point minX minY), (Point maxX maxY)) = do
  putStrLn "4"
  putStrLn "0,0"
  putStrLn "1,0"
  putStrLn "1,1"
  putStrLn "0,1"
  putStrLn "1"
  putStrLn "4 0 1 2 3"
  putStrLn $ showPoint $ Point minX minY
  putStrLn $ showPoint $ Point maxX minY
  putStrLn $ showPoint $ Point maxX maxY
  putStrLn $ showPoint $ Point minX maxY

main :: IO ()
main = do
  contents <- getContents
  printSolution $ bbox $ parsePoly contents
