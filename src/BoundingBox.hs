module BoundingBox where

import Data.Ratio
import Common
import Parse
import Math
import Show

bbox :: Poly -> (Point, Point)
bbox points =
    (topLeft, bottomRight)
        where
          topLeft = Point { px = minX, py = minY }
          bottomRight = Point { px = maxX, py = maxY }
          minX = minimum $ map px points
          minY = minimum $ map py points
          maxX = maximum $ map px points
          maxY = maximum $ map py points

bbox' :: Silhouette -> (Point, Point)
bbox' ((PolyFill poly) : _) = bbox poly
bbox' (_ : xs) = bbox' xs

makeRectangleSolution :: Poly -> Solution
makeRectangleSolution points =
    if 
        (width >= 1) && (height >= 1)
    then
        makeSolution points
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
        minX = minimum $ map px points
        minY = minimum $ map py points
        maxX = maximum $ map px points
        maxY = maximum $ map py points
        width = maxX - minX
        height = maxY - minY
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
        enumeratePoints points = map (\(i,v) -> IndexedPoint { index = i, srcvertex = v, dstvertex = v }) $ zip [0..6] points
    in
        Solution { points = enumeratePoints [a, b, c, d, e, f]
                 , facets = [[0, 1, 4, 3], [1, 2, 5, 4]]
                   --, dst = enumeratePoints [a', b', c', d', e', f']
                 }

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
        enumeratePoints points = map (\(i,v) -> IndexedPoint { index = i, srcvertex = v, dstvertex = v }) $ zip [0..6] points
    in
        Solution { points = enumeratePoints [a, b, c, d, e, f]
                 , facets = [[0, 1, 3, 2], [2, 3, 5, 4]]
                 --,  dst = enumeratePoints [ a', b', c', d', e', f' ]
                 }


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
        enumeratePoints points = map (\(i,v) -> IndexedPoint { index = i, srcvertex = v, dstvertex = v }) $ zip [0..6] points
    in
        Solution { points = enumeratePoints [a, b, c, d, e, f, g, h, j]
                 , facets = [[0, 1, 4, 3], [1, 2, 5, 4], [3, 4, 7, 6], [4, 5, 8, 7]]
                 --, dst = enumeratePoints [ a', b', c', d', e', f', g', h', j' ]
                 }



makeSolution :: Poly -> Solution
makeSolution points =
    translate topLeft initSolution
        where
          (topLeft, bottomRight) = bbox points

parseFirstPoly :: String -> Poly
parseFirstPoly = polygon . head . filter isFillPoly . fst . parseSilhouette . lines
    where
      isFillPoly (PolyFill _) = True
      isFillPoly (PolyHole _) = False
