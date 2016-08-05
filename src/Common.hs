module Common where

import Data.Ratio

type Number = Ratio Integer

data Point = Point { px :: Number, py :: Number } deriving (Eq, Show)

type Poly = [Point]

type Silhouette = [Poly]

type Edge = (Point, Point)

type Skeleton = [Edge]

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
