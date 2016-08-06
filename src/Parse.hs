module Parse where

import Data.Ratio
import Common
import Figures

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
          parsePolygons 0 acc rest = (map mkPoly acc, rest)
          parsePolygons count acc rest =
              let (poly, nextRest) = parsePoly rest
              in  parsePolygons (count - 1) (poly : acc) nextRest
          mkPoly points | dotPolyArea points < 0 = PolyFill points
          mkPoly points = PolyHole points

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

parseProblem :: [String] -> Problem
parseProblem lines =
    Problem { silhouette = silhouette, skeleton = skeleton }
        where
          (silhouette, skeletonLines) = parseSilhouette lines
          (skeleton, []) = parseSkeleton skeletonLines

parseSource :: [String] -> ([IndexedPoint], [String])
parseSource (pointsCountStr : restLines) =
    ((map (\(ind, p) -> IndexedPoint { index = ind, srcvertex = p, dstvertex = p }) $ zip [0 ..] parsedPoints), otherLines)
        where
          parsedPoints = (map parsePoint pointLines)
          count = read pointsCountStr
          pointLines = take count restLines
          otherLines = drop count restLines

parseFacets :: [String] -> ([FacetPoly], [String])
parseFacets (polysCountStr : restLines) =
    (map parseFacetPoly polyLines, otherLines)
        where
          count = read polysCountStr
          polyLines = take count restLines
          otherLines = drop count restLines
          parseFacetPoly = tail . map read . words

parseDestination :: Int -> [String] -> ([IndexedPoint], [String])
parseDestination count restLines =
    ((map (\(ind, p) -> IndexedPoint { index = ind, srcvertex = p, dstvertex = p }) $ zip [0 ..] parsedPoints), otherLines)
        where
          parsedPoints = (map parsePoint pointLines)
          pointLines = take count restLines
          otherLines = drop count restLines

parseSolution :: [String] -> Solution
parseSolution lines =
    Solution { points = src, facets = facets }
        where
          (src, facetsLines) = parseSource lines
          (facets, dstLines) = parseFacets facetsLines
          (dst, []) = parseDestination (length src) dstLines

scoreFiles :: String -> String -> IO Double
scoreFiles problemFile solutionFile = do
  problemContents <- readFile problemFile
  solutionContents <- readFile solutionFile
  let problem = parseProblem $ lines problemContents
  let solution = parseSolution $ lines solutionContents
  score (silhouette problem) solution
