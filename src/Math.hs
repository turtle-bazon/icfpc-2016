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
    Point { px = -(px point), py = -(px point) }

translatePoint :: Point -> Point -> Point
translatePoint delta point =
    Point { px = (px delta) + (px point), py = (py delta) + (py point) }

rotatePoint :: Point -> Float -> Point -> Point
rotatePoint pivot angle =
    fromOrigin . rotate . toOrigin
        where
          toOrigin = translatePoint (negatePoint pivot)
          fromOrigin = translatePoint pivot
          rotate p = Point { px = ((px p) * (approx $ cos angle)) - ((py p) * (approx $ sin angle))
                           , py = ((py p) * (approx $ cos angle)) + ((px p) * (approx $ sin angle))
                           }

translate :: Point -> Solution -> Solution
translate delta solution =
    solution { dst = map (translatePoint delta) $ dst solution }

rotate :: Point -> Float -> Solution -> Solution
rotate pivot angle solution =
    solution { dst = map (rotatePoint pivot angle) $ dst solution }

fold :: Edge -> Solution -> Solution
fold segment solution =
    undefined
