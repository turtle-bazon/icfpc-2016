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

data IndexedPoint = IndexedPoint { index :: PointIndex, vertices :: [Point] } deriving (Eq, Show)

type FacetPoly = [PointIndex]

data Solution = Solution { src :: [IndexedPoint], facets :: [FacetPoly], dst :: [Point] } deriving (Eq, Show)


polygon :: SilhouettePoly -> Poly
polygon (PolyFill points) = points
polygon (PolyHole points) = points

polyArea :: Poly -> Number
polyArea poly =
    (sum chunks) / 2
        where
          chunks = zipWith multiply poly (tail $ cycle poly)
          multiply p1 p2 = ((px p1) + (px p2)) * ((py p1) - (py p2))

silhouetteArea :: Silhouette -> Number
silhouetteArea =
    foldr sumArea 0
        where
          sumArea (PolyFill poly) total = total + (abs $ polyArea poly)
          sumArea (PolyHole poly) total = total - (abs $ polyArea poly)
