module BoundingBox where

import Data.Ratio
import Common
import Parse
import Show

bbox :: Poly -> (Point, Point)
bbox points =
    (topLeft, bottomRight)
        where
          topLeft = Point { px = minX, py = minY }
          bottomRight = Point { px = minX + side, py = minY + side }
          -- side = max (maxX - minX) (maxY - minY)
          side = 1 % 1
          minX = minimum $ map px points
          minY = minimum $ map py points
          maxX = maximum $ map px points
          maxY = maximum $ map py points

makeSolution :: (Point, Point) -> Solution
makeSolution (tl, br) =
    Solution { src = [ IndexedPoint { index = 0, vertex = Point { px = 0, py = 0 } }
                     , IndexedPoint { index = 1, vertex = Point { px = 1, py = 0 } }
                     , IndexedPoint { index = 2, vertex = Point { px = 1, py = 1 } }
                     , IndexedPoint { index = 3, vertex = Point { px = 0, py = 1 } }
                     ],
               facets = [[0, 1, 2, 3]],
               dst = [ Point (px tl) (py tl)
                     , Point (px br) (py tl)
                     , Point (px br) (py br)
                     , Point (px tl) (py br)
                     ] }

parseFirstPoly :: String -> Poly
parseFirstPoly = polygon . head . filter isFillPoly . fst . parseSilhouette . lines
    where
      isFillPoly (PolyFill _) = True
      isFillPoly (PolyHole _) = False
