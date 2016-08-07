{-# LANGUAGE BangPatterns #-}

module Common where

import Data.List
import Data.Maybe
import Data.Ratio

type Number = Ratio Integer

data Point = Point { px :: !Number, py :: !Number } deriving (Eq, Ord, Show)

type Poly = [Point]

data SilhouettePoly = PolyFill !Poly | PolyHole !Poly deriving (Eq, Show)

type Silhouette = [SilhouettePoly]

type Edge = (Point, Point)

data Orientation = Collinear | Clockwise | CounterClockwise deriving (Eq, Show)

type Skeleton = [Edge]

data Problem = Problem { silhouette :: !Silhouette, skeleton :: !Skeleton } deriving (Eq, Show)

type PointIndex = Int

data IndexedPoint = IndexedPoint { index :: PointIndex, srcvertex :: Point, dstvertex :: Point } deriving (Eq, Show)

type FacetPoly = [PointIndex]

data Solution = Solution { points :: [IndexedPoint], facets :: [FacetPoly] } deriving (Eq, Show)

initSolution :: Solution
initSolution =
    Solution { points = points
             , facets = [[0, 1, 2, 3]] }
        where
          points = [ IndexedPoint { index = 0, srcvertex = Point { px = 0, py = 0 }, dstvertex = Point { px = 0, py = 0 } }
                   , IndexedPoint { index = 1, srcvertex = Point { px = 1, py = 0 }, dstvertex = Point { px = 1, py = 0 } }
                   , IndexedPoint { index = 2, srcvertex = Point { px = 1, py = 1 }, dstvertex = Point { px = 1, py = 1 } }
                   , IndexedPoint { index = 3, srcvertex = Point { px = 0, py = 1 }, dstvertex = Point { px = 0, py = 1 } }
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
          restorePoint idx = dstvertex $ fromJust $ find ((== idx) . index) $ points sol

facetsPolys' :: Solution -> [[IndexedPoint]]
facetsPolys' sol =
    map restorePoly $ facets sol
        where
          restorePoly indices = map restorePoint indices
          restorePoint idx = fromJust $ find ((== idx) . index) $ points sol
