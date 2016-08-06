module Common where

import Data.List
import Data.Maybe
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

data Solution = Solution { src :: [IndexedPoint], facets :: [FacetPoly], dst :: [IndexedPoint] } deriving (Eq, Show)

initSolution :: Solution
initSolution =
    Solution { src = points,
               facets = [[0, 1, 2, 3]],
               dst = points
             }
        where
          points = [ IndexedPoint { index = 0, vertex = Point { px = 0, py = 0 } }
                   , IndexedPoint { index = 1, vertex = Point { px = 1, py = 0 } }
                   , IndexedPoint { index = 2, vertex = Point { px = 1, py = 1 } }
                   , IndexedPoint { index = 3, vertex = Point { px = 0, py = 1 } }
                   ]

polygon :: SilhouettePoly -> Poly
polygon (PolyFill points) = points
polygon (PolyHole points) = points

dotPolyArea :: Poly -> Number
dotPolyArea poly =
    (sum chunks) / 2
        where
          chunks = zipWith multiply poly (tail $ cycle poly)
          multiply p1 p2 = ((px p1) + (px p2)) * ((py p1) - (py p2))

facetsPolys :: Solution -> [Poly]
facetsPolys sol =
    map restorePoly $ facets sol
        where
          restorePoly indices = map restorePoint indices
          restorePoint idx = vertex $ fromJust $ find ((== idx) . index) $ dst sol
