module BoundingBox where

import Data.Ratio
import Common
import Parse
import Math
import Show

bbox :: Poly -> (Point, Point)
bbox points =
    (topLeft, bottomRight)
        where
          topLeft = Point { px = minX, py = minY }
          bottomRight = Point { px = maxX, py = maxY }
          minX = minimum $ map px points
          minY = minimum $ map py points
          maxX = maximum $ map px points
          maxY = maximum $ map py points

makeSolution :: Poly -> Solution
makeSolution points =
    translate topLeft initSolution
        where
          (topLeft, bottomRight) = bbox points

parseFirstPoly :: String -> Poly
parseFirstPoly = polygon . head . filter isFillPoly . fst . parseSilhouette . lines
    where
      isFillPoly (PolyFill _) = True
      isFillPoly (PolyHole _) = False
