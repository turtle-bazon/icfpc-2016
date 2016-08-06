module SolverBBSimple (solverBBSimple, bbox, parseFirstPoly, makeBBSolution) where

import Data.Ratio
import Common
import Math

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

makeBBSolution :: Poly -> Solution
makeBBSolution points =
    translate topLeft initSolution
        where
          (topLeft, bottomRight) = bbox points

parseFirstPoly :: Silhouette -> Poly
parseFirstPoly = polygon . head . filter isFillPoly
    where
      isFillPoly (PolyFill _) = True
      isFillPoly (PolyHole _) = False

solverBBSimple :: Problem -> Solution
solverBBSimple = makeBBSolution . parseFirstPoly . silhouette
