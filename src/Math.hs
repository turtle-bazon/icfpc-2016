module Math where

import Data.Ratio
import Common
import Show

epsilon :: Float
epsilon = 1.0 - 0.999999

approx :: Float -> Number
approx = flip approxRational $ epsilon

negatePoint :: Point -> Point
negatePoint point =
    Point { px = -(px point), py = -(py point) }

translatePoint :: Point -> IndexedPoint -> IndexedPoint
translatePoint delta (IndexedPoint index srcvertex dstvertex) =
  IndexedPoint { index = index
               , srcvertex = srcvertex
               , dstvertex = Point { px = (px delta) + (px dstvertex), py = (py delta) + (py dstvertex) }}
    
rotatePoint :: Point -> Float -> IndexedPoint -> IndexedPoint
rotatePoint pivot angle =
  fromOrigin . rotate . toOrigin
  where
    toOrigin = translatePoint (negatePoint pivot)
    fromOrigin = translatePoint pivot
    rotate (IndexedPoint index srcvertex dstvertex) =
      IndexedPoint { index = index
                   , srcvertex = srcvertex
                   , dstvertex = Point { px = ((px dstvertex) * (approx $ cos angle)) - ((py dstvertex) * (approx $ sin angle))
                                       , py = ((py dstvertex) * (approx $ cos angle)) + ((px dstvertex) * (approx $ sin angle))
                                       }}

translate :: Point -> Solution -> Solution
translate delta solution =
    solution { points = map (translatePoint delta) $ points solution }
    
rotate :: Point -> Float -> Solution -> Solution
rotate pivot angle solution =
    solution { points = map (rotatePoint pivot angle) $ points solution }
    
fold :: Edge -> Solution -> Solution
fold segment solution =
    undefined
