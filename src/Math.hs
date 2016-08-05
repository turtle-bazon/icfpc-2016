module Math where

import Common
import Show

translatePoint :: Point -> Point -> Point
translatePoint delta point =
    Point { px = (px delta) + (px point), py = (py delta) + (py point) }

translate :: Point -> Solution -> Solution
translate delta solution =
    solution { dst = map (translatePoint delta) $ dst solution }

rotate :: Float -> Solution -> Solution
rotate angle solution =
    undefined

fold :: Edge -> Solution -> Solution
fold segment solution =
    undefined
