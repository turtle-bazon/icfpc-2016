module SolverBBRotate (solverBBRotate) where

import qualified Data.Number.FixedFunctions as FF
import Common
import Math
import SolverBBSimple
import SolverBBRect

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
                  other -> FF.atan epsilonN $ abs $ (y1-y2)/other
        (fsin, fcos) = calcTrig (-alpha)
        rotated = map (rotatePoint' (Point 0 0) fsin fcos) $ map (\(x,y) -> Point x y) points

        minX = minimum $ map px rotated
        minY = minimum $ map py rotated
        maxX = maximum $ map px rotated
        maxY = maximum $ map py rotated
        width = maxX - minX
        height = maxY - minY

        polygons' = map (\p -> map (rotatePoint' (Point 0 0) fsin fcos) p) polygons
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

solverBBRotate :: Problem -> IO Solution
solverBBRotate = return . makeRectangleSolution
