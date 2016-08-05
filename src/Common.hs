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

showNumber :: Number -> String
showNumber v =
    case denominator v of
      1 -> show $ numerator v
      d -> (show $ numerator v) ++ "/" ++ (show d)

showPoint :: Point -> String
showPoint p =
    (showNumber $ px p) ++ "," ++ (showNumber $ py p)

showEdge :: Edge -> String
showEdge (a, b) =
    (showPoint a) ++ " " ++ (showPoint b)

polyArea :: Poly -> Number
polyArea poly =
    (sum chunks) / 2
        where
          chunks = zipWith multiply poly (tail $ cycle poly)
          multiply p1 p2 = ((px p1) + (px p2)) * ((py p1) - (py p2))
