module Common where

import Data.Ratio

type Number = Ratio Integer

data Point = Point { px :: Number, py :: Number } deriving (Eq, Show)

type Poly = [Point]

data SilhouettePoly = PolyFill Poly | PolyHole Poly deriving (Eq, Show)

type Silhouette = [SilhouettePoly]

type Edge = (Point, Point)

type Skeleton = [Edge]

data Problem = Problem { silhouette :: Silhouette, skeleton :: Skeleton } deriving (Eq, Show)

type PointIndex = Int

data IndexedPoint = IndexedPoint { index :: PointIndex, vertex :: Point } deriving (Eq, Show)

type FacetPoly = [PointIndex]

data Solution = Solution { src :: [IndexedPoint], facets :: [FacetPoly], dst :: [Point] } deriving (Eq, Show)

initSolution :: Solution
initSolution =
    Solution { src = [ IndexedPoint { index = 0, vertex = Point { px = 0, py = 0 } }
                     , IndexedPoint { index = 1, vertex = Point { px = 1, py = 0 } }
                     , IndexedPoint { index = 2, vertex = Point { px = 1, py = 1 } }
                     , IndexedPoint { index = 3, vertex = Point { px = 0, py = 1 } }
                     ],
               facets = [[0, 1, 2, 3]],
               dst = [ Point { px = 0, py = 0 }
                     , Point { px = 1, py = 0 }
                     , Point { px = 1, py = 1 }
                     , Point { px = 0, py = 1 }
                     ] }

polygon :: SilhouettePoly -> Poly
polygon (PolyFill points) = points
polygon (PolyHole points) = points

