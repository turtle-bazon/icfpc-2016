module Parse where

import Data.Ratio
import Common

parseNumber :: String -> Number
parseNumber string =
    numerator % denominator
        where
          numerator = read $ takeWhile ((/=) '/') string
          denominator = parseDenominatorString $ dropWhile ((/=) '/') string
          parseDenominatorString "" = 1
          parseDenominatorString other = read $ tail $ other

parsePoint :: String -> Point
parsePoint line =
    Point { px = x, py = y }
        where
          x = parseNumber $ takeWhile ((/=) ',') line
          y = parseNumber $ tail $ dropWhile ((/=) ',') line

parsePoly :: [String] -> (Poly, [String])
parsePoly (edgesCountStr : restLines) =
    (map parsePoint polyLines, otherLines)
        where
          count = read edgesCountStr
          polyLines = take count restLines
          otherLines = drop count restLines

parseSilhouette :: [String] -> (Silhouette, [String])
parseSilhouette (polyCountStr : restLines) =
    parsePolygons (read polyCountStr) [] restLines
        where
          parsePolygons 0 acc rest = (acc, rest)
          parsePolygons count acc rest =
              let (poly, nextRest) = parsePoly rest
              in  parsePolygons (count - 1) (poly : acc) nextRest

parseEdge :: String -> Edge
parseEdge line =
    (pointA, pointB)
        where
          pointA : pointB : [] = map parsePoint $ words line

parseSkeleton :: [String] -> (Skeleton, [String])
parseSkeleton (edgesCountStr : restLines) =
    (map parseEdge edgeLines, otherLines)
        where
          count = read edgesCountStr
          edgeLines = take count restLines
          otherLines = drop count restLines
