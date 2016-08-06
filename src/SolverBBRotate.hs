module SolverBBRotate (solverBBRotate) where

import Common
import Math
import SolverBBSimple

dist (x1,y1) (x2,y2) =
    (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)

bigestDistance (p1:p2:ps) =
    bigestDistance' (p1:p2:ps) (p1,p2)

bigestDistance' [] res = res
bigestDistance' (p:[]) res = res
bigestDistance' (p1:p2:ps) (a,b) =
    let
        res1 = if (dist p1 p2) > (dist a b) then (p1,p2) else (a,b)
        res2 = bigestDistance' (p2:ps) res1
    in
        res2

makeRectangleSolution :: Problem -> Solution
makeRectangleSolution problem =
    let
        polygons = map polygon $ filter isFillPoly $ silhouette problem
        (pxs,pys) = foldl (\(xs,ys) -> \p -> ((map px p) ++ xs, (map py p) ++ ys)) ([],[]) polygons

        points = zip pxs pys
        ((x1,y1),(x2,y2)) = bigestDistance $ points
        alpha = case x1 - x2 of
                  0 -> 0
                  other -> atan $ fromRational $ abs $ (y1-y2)/other
        rotated = map (rotatePoint (Point 0 0) (-alpha)) $ map (\(x,y) -> Point x y) points

        minX = minimum $ map px rotated
        minY = minimum $ map py rotated
        maxX = maximum $ map px rotated
        maxY = maximum $ map py rotated
        width = maxX - minX
        height = maxY - minY

        polygons' = map (\p -> map (rotatePoint (Point 0 0) (-alpha)) p) polygons



    in
        rotate (Point 0 0) (alpha) $ makeRectangleSolution' width height minX minY (head polygons')
    where
        isFillPoly (PolyFill _) = True
        isFillPoly (PolyHole _) = False


makeRectangleSolution' :: Number -> Number -> Number -> Number -> Poly -> Solution
makeRectangleSolution' width height minX minY points =
    if
        (width >= 1) && (height >= 1)
    then
        makeBBSolution points
    else
        if
            width >= 1
        then
            translate (Point minX minY) rectH
        else
            if
                height >= 1
            then
                translate (Point minX minY) rectW
            else
                translate (Point minX minY) rect
    where
        rect = foldRectangle width height
        rectH = foldRectangleH height
        rectW = foldRectangleW width

foldRectangleW :: Number -> Solution
foldRectangleW width =
    let
        a = Point 0 0
        b = Point width 0
        c = Point 1 0
        d = Point 0 1
        e = Point width 1
        f = Point 1 1

        a' = Point 0 0
        b' = Point width 0
        c' = Point (2*width - 1) 0
        d' = Point 0 1
        e' = Point width 1
        f' = Point (2*width - 1) 1
    in
        Solution { src = map (\(i,v) -> IndexedPoint { index = i, vertex = v }) $ zip [0..] [a, b, c, d, e, f],
                   facets = [[0, 1, 4, 3], [1, 2, 5, 4]],
                   dst = zipWith IndexedPoint [0 ..] [ a', b', c', d', e', f' ] }

foldRectangleH :: Number -> Solution
foldRectangleH height =
    let
        a = Point 0 0
        b = Point 1 0
        c = Point 0 height
        d = Point 1 height
        e = Point 0 1
        f = Point 1 1

        a' = Point 0 0
        b' = Point 1 0
        c' = Point 0 height
        d' = Point 1 height
        e' = Point 0 (2*height - 1)
        f' = Point 1 (2*height - 1)
    in
        Solution { src = map (\(i,v) -> IndexedPoint { index = i, vertex = v }) $ zip [0..] [a, b, c, d, e, f],
                   facets = [[0, 1, 3, 2], [2, 3, 5, 4]],
                   dst = zipWith IndexedPoint [0 ..] [ a', b', c', d', e', f' ] }


foldRectangle :: Number -> Number -> Solution
foldRectangle width height =
    let
        a = Point 0 0
        b = Point width 0
        c = Point 1 0
        d = Point 0 height
        e = Point width height
        f = Point 1 height
        g = Point 0 1
        h = Point width 1
        j = Point 1 1

        a' = Point 0 0
        b' = Point width 0
        c' = Point (2*width - 1) 0
        d' = Point 0 height
        e' = Point width height
        f' = Point (2*width - 1) height
        g' = Point 0 (2*height - 1)
        h' = Point width (2*height - 1)
        j' = Point (2*width - 1) (2*height - 1)
    in
        Solution { src = map (\(i,v) -> IndexedPoint { index = i, vertex = v }) $ zip [0..] [a, b, c, d, e, f, g, h, j],
                   facets = [[0, 1, 4, 3], [1, 2, 5, 4], [3, 4, 7, 6], [4, 5, 8, 7]],
                   dst = zipWith IndexedPoint [0 ..] [ a', b', c', d', e', f', g', h', j' ] }

solverBBRotate :: Problem -> Solution
solverBBRotate = makeRectangleSolution
