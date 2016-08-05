module Figures where

import Data.Ratio
import Data.SG.Geometry.TwoDim
import Common
import Math

type GPoint = Point2' Number
type GEdge = Line2' Number
type GPoly = [Line2' Number]

pointToGPoint :: Point -> GPoint
pointToGPoint p =
    Point2 ((px p), (py p))

gpointToPoint :: GPoint -> Point
gpointToPoint (Point2 (x, y)) =
    Point { px = x, py = y }

edgeToGEdge :: Edge -> GEdge
edgeToGEdge (a, b) =
    Line2 (pointToGPoint a) $ makeRel2 ((px p), (py p))
        where
          p = translatePoint (negatePoint a) b

gedgeToEdge :: GEdge -> Edge
gedgeToEdge ge =
    (start, translatePoint dir start)
        where
          start = gpointToPoint $ getLineStart2 ge
          Rel2 gdir _ = getLineDir2 ge
          dir = gpointToPoint $ Point2 gdir

polyToGPoly :: Poly -> GPoly
polyToGPoly poly =
    map edgeToGEdge $ zip poly (tail $ cycle poly)

gpolyToPoly :: GPoly -> Poly
gpolyToPoly =
    map (fst . gedgeToEdge)
