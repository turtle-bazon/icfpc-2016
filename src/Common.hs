module Common where

import Data.Ratio

type Number = Ratio Integer

data Point = Point { px :: Number, py :: Number } deriving (Eq, Show)

type Poly = [Point]

data SilhouettePoly = PolyFill Poly | PolyHole Poly deriving (Eq, Show)

type Silhouette = [SilhouettePoly]

type Edge = (Point, Point)

data Line = Line {k :: Number, b :: Number}

lineForEdge :: Edge -> Line
lineForEdge edge =
  let (p1, p2) = edge
      k = (py p2 - py p1) / (px p2 - py p1)
      b = px p2 - k * px p2
  in
    Line {k = k,
          b = b}

normalLineBy :: Point -> Line -> Line
normalLineBy (Point px py) (Line k b) =
  let b = py + k * px
  in Line {k = (-k),
           b = b}

isPointPosition :: Point -> Line -> (Number -> Number -> Bool) -> Bool
isPointPosition (Point x y) (Line k b) sideComparator =
  y `sideComparator` (k * x + b)

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

reflectPointBy :: Point -> Line -> Point
reflectPointBy (Point px py) (Line k b) =
  let d = (px + (py - b) * k) / (1 + k^2)
      refx = 2 * d - px
      refy = 2 * d * k - py + 2 * b
  in Point {px = refx, py = refy}
