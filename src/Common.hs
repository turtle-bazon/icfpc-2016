module Common where

import Data.Ratio
import Text.JSON

type Number = Ratio Integer

data Point = Point { px :: Number, py :: Number } deriving (Eq, Show)

instance JSON Point where
  readJSON = undefined
  showJSON Point {px = px, py = py} = JSArray [JSRational True px, JSRational True py]

type Poly = [Point]

data SilhouettePoly = PolyFill Poly | PolyHole Poly deriving (Eq, Show)

instance JSON SilhouettePoly where
  readJSON = undefined
  showJSON (PolyFill points) = showJSON points
  showJSON (PolyHole points) = JSArray []
  -- JSObject (JSArray [ (toJSString "type", toJSString "fill"), (toJSString "points", showJSON points) ])
  -- showJSON (PolyHole points) = toJSObject (JSObject [ ("type", "hole"), ("points", points) ]

type Silhouette = [SilhouettePoly]

type Edge = (Point, Point)

type Skeleton = [Edge]

data Problem = Problem { silhouette :: Silhouette, skeleton :: Skeleton } deriving (Eq, Show)

instance JSON Problem where
  readJSON = undefined
  showJSON Problem {silhouette = silhouette} = showJSON silhouette

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
