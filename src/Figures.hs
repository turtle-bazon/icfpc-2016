module Figures where

import Data.Ratio
import Data.Geometry.Point
import Data.Geometry.Line
import Data.Geometry.SetOperations
import Common

type GPoint = Point2' Number

type GEdge = LineSegment2' Number

type GPoly = Polyline2' Number

pointToPoint2 :: Point -> GPoint
pointToPoint2 p =
    Point2 ((px p), (py p))

edgeToGEdge :: Edge -> GEdge
edgeToGEdge (a, b) =
    LineSegment2 (pointToPoint2 a) (pointToPoint2 b)

polyToGPoly :: Poly -> GPoly
polyToGPoly poly =
    Polyline2 $ map edgeToGEdge $ zip poly (tail $ cycle poly)
