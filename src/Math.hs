module Math where

import Data.List
import Data.Maybe
import Data.Ratio
import qualified Data.Number.FixedFunctions as FF
import Common
import Show

epsilon :: Double
epsilon = 1.0 - 0.999999999

epsilonN :: Number
epsilonN = 1 % 1000000000

approx :: Double -> Number
approx = flip approxRational $ epsilon

negatePoint :: Point -> Point
negatePoint point =
    Point { px = -(px point), py = -(py point) }

translatePoint :: Point -> Point -> Point
translatePoint delta point =
    Point { px = (px delta) + (px point), py = (py delta) + (py point) }

calcTrig :: Number -> (Number, Number)
calcTrig angle = (FF.sin epsilonN angle, FF.cos epsilonN angle)

rotatePoint :: Point -> Number -> Point -> Point
rotatePoint pivot angle =
    rotatePoint' pivot fsin fcos
        where
          (fsin, fcos) = calcTrig angle

rotatePoint' :: Point -> Number -> Number -> Point -> Point
rotatePoint' pivot fsin fcos =
    fromOrigin . rotate . toOrigin
        where
          toOrigin = translatePoint (negatePoint pivot)
          fromOrigin = translatePoint pivot
          rotate p = Point { px = ((px p) * fcos) - ((py p) * fsin)
                           , py = ((py p) * fcos) + ((px p) * fsin)
                           }

rotatePointTo :: Point -> Edge -> Point -> Point
rotatePointTo pivot ((Point x1 y1), (Point x2 y2)) =
  fromOrigin . rotate . toOrigin
  where
    toOrigin = translatePoint (negatePoint pivot)
    fromOrigin = translatePoint pivot
    rotate p = let d2 = (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)
                   d = approx $ sqrt $ fromRational d2
                   dx = x2 - x1
                   dy = y2 - y1
                   cosangle = dx / d
                   sinangle = dy / d
               in Point { px = ((px p) * cosangle) - ((py p) * sinangle)
                        , py = ((py p) * cosangle) + ((px p) * sinangle)
                         }

{-- Function computes slope by two points--}
slope :: Point -> Point -> Number
slope (Point x1 y1) (Point x2 y2) = (y1 - y2) / (x1 - x2)

{-- Function computes slope/intercept form of a line equation --}
lineEquationByPoints :: Point -> Point -> (Number, Number)
lineEquationByPoints p1@(Point x1 y1) p2 =
  let m = slope p1 p2 in (m, y1 - m * x1)

{-- Functions reflect point over a line given by edge.
    Note: length of edge is not checked, so use carefuly --}
reflectPointOverEdge :: Point -> Edge -> Point
reflectPointOverEdge (Point x y) (p1, p2) =
  let (a, c) = lineEquationByPoints p1 p2
      d = (x + (y - c) * a) / (1 + a ^ 2)
  in Point (2 * d - x) (2 * d * a - y + 2 * c)

reflectEdgeOverEdge :: Edge -> Edge -> Edge
reflectEdgeOverEdge (p1, p2) e = (reflectPointOverEdge p1 e, reflectPointOverEdge p2 e)

{-- Given three colinear points p, q, r, the function checks if
    point q lies on line segment 'pr' --}
isOnSegment :: Point -> Edge -> Bool
isOnSegment (Point qx qy) (Point px py, Point rx ry) =
  let x_check = (qx <= max px rx) && (qx >= min px rx)
      y_check = (qy <= max py ry) && (qy >= min py ry)
  in (x_check && y_check)

{-- Function finds orientation of ordered triplet (p, q, r).
    See http://www.geeksforgeeks.org/orientation-3-ordered-points/
    for details of below formula --}
orientation :: Point -> Point -> Point -> Orientation
orientation (Point px py) (Point qx qy) (Point rx ry) =
  let val = (qy - py) * (rx - qx) - (qx - px) * (ry - qy)
  in case val of _
                   | val == 0 -> Collinear
                   | val >  0 -> Clockwise
                   | val <  0 -> CounterClockwise

{-- Function returns true if edges 'p1q1' and 'p2q2' intersect.
    Note: since collinear edges are of no interest to us, we do
    not check such intersections --}
doIntersect :: Edge -> Edge -> Bool
doIntersect (p1, q1) (p2, q2) =
  let o1 = orientation p1 q1 p2
      o2 = orientation p1 q1 q2
      o3 = orientation p2 q2 p1
      o4 = orientation p2 q2 q1
  in (o1 /= o2 && o3 /= o4)

{-- Simple vector functions --}
addVectors :: Point -> Point -> Point
addVectors (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

substractVectors :: Point -> Point -> Point
substractVectors (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

cross :: Point -> Point -> Number
cross (Point x1 y1) (Point x2 y2) = x1 * y2 - y1 * x2

scalarProduct :: Number -> Point -> Point
scalarProduct m (Point x y) = Point (m * x) (m * y)

{-- Function that computes intersection point of two segments. It has almost no checks,
    so it is encouraged to use doIntersect first, otherwise result may be bogus --}
findIntersection :: Edge -> Edge -> Maybe Point
findIntersection (p1, p2) (q1, q2) =
  let r = substractVectors p2 p1
      s = substractVectors q2 q1
      rxs = cross r s
      qp = substractVectors q1 p1
      t = (cross qp s) / rxs
      u = (cross qp r) / rxs
  in if ((rxs /= 0) && (0 <= t && t <= 1) && (0 <= u && u <= 1))
     then Just (addVectors p1 (scalarProduct t r))
     else Nothing

{-- Following functions break edge in two in case it intersects with other edge --}
checkAndBreakEdge :: Edge -> Edge -> Either (Edge, Edge) Edge
checkAndBreakEdge edge1 edge2 = if (doIntersect edge1 edge2)
                                then breakEdge edge1 edge2
                                else Right edge1

breakEdge :: Edge -> Edge -> Either (Edge, Edge) Edge
breakEdge edge1@(p1, p2) edge2 = case findIntersection edge1 edge2 of
                             Nothing -> Right edge1
                             Just x ->  Left ((p1, x), (x, p2))

{-- Function filters out points on one side of a line defined by two points. Side is
    chosen with regards to points number - smaller number to fold is preferred --}
filterPointsByLine :: (Point, Point) -> [Point] -> [Point]
filterPointsByLine (Point x1 y1, Point x2 y2) points =
  let check (Point x y)         = ((x2 - x1) * (y - y1) - (y2 - y1) * (x - x1)) > 0
      (leftPoints, rightPoints) = partition check points
      leftSize                  = length leftPoints
      rightSize                 = length rightPoints
  in if leftSize > rightSize then leftPoints else rightPoints

translate :: Point -> Solution -> Solution
translate delta solution =
    solution { points = map translateDst $ points solution }
        where
          translateDst p = p { dstvertex = translatePoint delta $ dstvertex p }

rotate :: Point -> Number -> Solution -> Solution
rotate pivot angle solution =
    solution { points = map rotateDst $ points solution }
        where
          (fsin, fcos) = calcTrig angle
          rotateDst p = p { dstvertex = rotatePoint' pivot fsin fcos $ dstvertex p }

rotateTo :: Point -> Edge -> Solution -> Solution
rotateTo pivot edge solution =
    solution { points = map rotateDst $ points solution }
        where
          rotateDst p = p { dstvertex = rotatePointTo pivot edge $ dstvertex p }

fold :: Edge -> Solution -> Solution
fold segment solution =
    undefined

facetIdxToPoints :: [IndexedPoint] -> FacetPoly -> [IndexedPoint]
facetIdxToPoints points facet =
  map (\ind -> fromJust (find ((== ind) . index) points)) facet

isFacetForTransformingDown :: Number -> [IndexedPoint] -> FacetPoly -> Bool
isFacetForTransformingDown y points facet =
  let facetPoints = facetIdxToPoints points facet
      facetYs = (map (\ip -> py $ dstvertex ip) facetPoints)
  in y < (maximum facetYs)

isFacetForTransformingUp :: Number -> [IndexedPoint] -> FacetPoly -> Bool
isFacetForTransformingUp y points facet =
  let facetPoints = facetIdxToPoints points facet
      facetYs = (map (\ip -> py $ dstvertex ip) facetPoints)
  in y > (minimum facetYs)

isFacetForTransformingRight :: Number -> [IndexedPoint] -> FacetPoly -> Bool
isFacetForTransformingRight x points facet =
  let facetPoints = facetIdxToPoints points facet
      facetXs = (map (\ip -> px $ dstvertex ip) facetPoints)
  in x > (minimum facetXs)

isFacetForTransformingLeft :: Number -> [IndexedPoint] -> FacetPoly -> Bool
isFacetForTransformingLeft x points facet =
  let facetPoints = facetIdxToPoints points facet
      facetXs = (map (\ip -> px $ dstvertex ip) facetPoints)
  in x < (maximum facetXs)

isEdgeCrossingHorizontal :: Number -> (IndexedPoint, IndexedPoint) -> Bool
isEdgeCrossingHorizontal y (p1, p2) =
  let maxy = max (py (dstvertex p1)) (py (dstvertex p2))
      miny = min (py (dstvertex p1)) (py (dstvertex p2))
  in (miny < y) && (y < maxy)

isEdgeCrossingVertical :: Number -> (IndexedPoint, IndexedPoint) -> Bool
isEdgeCrossingVertical x (p1, p2) =
  let maxx = max (px (dstvertex p1)) (px (dstvertex p2))
      minx = min (px (dstvertex p1)) (px (dstvertex p2))
  in (minx < x) && (x < maxx)

isFacetCrossingHorizontal :: Number -> [IndexedPoint] -> FacetPoly -> Bool
isFacetCrossingHorizontal y points facet =
  let [p1, p2, p3, p4] = (facetIdxToPoints points facet)
  in or $ map (isEdgeCrossingHorizontal y) [(p1, p2), (p2, p3), (p3, p4), (p4, p1)]

isFacetCrossingVertical :: Number -> [IndexedPoint] -> FacetPoly -> Bool
isFacetCrossingVertical x points facet =
  let [p1, p2, p3, p4] = (facetIdxToPoints points facet)
  in or $ map (isEdgeCrossingVertical x) [(p1, p2), (p2, p3), (p3, p4), (p4, p1)]

tryTransformPointDown :: Number -> IndexedPoint -> IndexedPoint
tryTransformPointDown y (IndexedPoint idx srcv dstv) =
  if y < (py dstv)
  then IndexedPoint { index = idx
                    , srcvertex = srcv
                    , dstvertex = Point { px = px dstv, py = 2 * y - (py dstv) }}
  else IndexedPoint { index = idx
                    , srcvertex = srcv
                    , dstvertex = dstv }

tryTransformPointUp :: Number -> IndexedPoint -> IndexedPoint
tryTransformPointUp y (IndexedPoint idx srcv dstv) =
  if y > (py dstv)
  then IndexedPoint { index = idx
                    , srcvertex = srcv
                    , dstvertex = Point { px = px dstv, py = 2 * y - (py dstv) }}
  else IndexedPoint { index = idx
                    , srcvertex = srcv
                    , dstvertex = dstv }

tryTransformPointRight :: Number -> IndexedPoint -> IndexedPoint
tryTransformPointRight x (IndexedPoint idx srcv dstv) =
  if x > (px dstv)
  then IndexedPoint { index = idx
                    , srcvertex = srcv
                    , dstvertex = Point { px = 2 * x - (px dstv), py = py dstv }}
  else IndexedPoint { index = idx
                    , srcvertex = srcv
                    , dstvertex = dstv }

tryTransformPointLeft :: Number -> IndexedPoint -> IndexedPoint
tryTransformPointLeft x (IndexedPoint idx srcv dstv) =
  if x < (px dstv)
  then IndexedPoint { index = idx
                    , srcvertex = srcv
                    , dstvertex = Point { px = 2 * x - (px dstv), py = py dstv }}
  else IndexedPoint { index = idx
                    , srcvertex = srcv
                    , dstvertex = dstv }

splitByChecker :: [(IndexedPoint, IndexedPoint)] -> ((IndexedPoint, IndexedPoint) -> Bool) -> ([IndexedPoint], [IndexedPoint])
splitByChecker ((p1, p2) : restPairs) checker =
  if (checker (p1, p2))
  then
    let (pa, pb) = (head restPairs)
        (pc, pd) = (head (tail (tail restPairs)))
    in ([pa, pb], [pc, pd])
  else splitByChecker (restPairs ++ [(p1, p2)]) checker

createHorizontalDownFacets :: Number -> [IndexedPoint] -> [IndexedPoint] -> [FacetPoly] -> [FacetPoly] -> ([IndexedPoint], [FacetPoly])
createHorizontalDownFacets _ points newPoints newFacets [] =
  (newPoints, newFacets)
createHorizontalDownFacets y points newPoints newFacets (currentFacet:restFacets) =
  if isFacetForTransformingDown y points currentFacet
  then
    let freshPoints = map (\ip -> if (elem (index ip) currentFacet)
                                  then tryTransformPointDown y ip
                                  else ip) newPoints
    in if isFacetCrossingHorizontal y points currentFacet
       then
         let maxId = maximum $ map index newPoints
             id1 = maxId + 1
             id2 = maxId + 2
             [p1, p2, p3, p4] = (facetIdxToPoints points currentFacet)
             ([pa, pb], [pc, pd]) = splitByChecker [(p1, p2), (p2, p3), (p3, p4), (p4, p1)] (isEdgeCrossingHorizontal y)
             x1 = (px (dstvertex pa))
             x2 = (px (dstvertex pb))
             y1 = y
             y2 = y
             dy = abs (y - (py (dstvertex pd)))
             newp1 = IndexedPoint { index = id1
                                  , srcvertex = Point { px = (px (srcvertex pa)),
                                                        py = (py (srcvertex pd)) + signum ((py (srcvertex pa)) - (py (srcvertex pd))) * dy }
                                  , dstvertex = Point { px = x1, py = y1 } }
             newp2 = IndexedPoint { index = id2
                                  , srcvertex = Point { px = (px (srcvertex pc)),
                                                        py = (py (srcvertex pc)) + signum ((py (srcvertex pb)) - (py (srcvertex pc))) * dy }
                                  , dstvertex = Point { px = x2, py = y2 }}
             newFacet1 = [(index pa), (index pb), id2, id1]
             newFacet2 = [id1, id2, (index pc), (index pd)]
         in
           createHorizontalDownFacets y points (newp1 : newp2 : freshPoints) (newFacet1 : newFacet2 : newFacets) restFacets
       else
         createHorizontalDownFacets y points freshPoints (currentFacet : newFacets) restFacets
  else createHorizontalDownFacets y points newPoints (currentFacet : newFacets) restFacets

createHorizontalUpFacets :: Number -> [IndexedPoint] -> [IndexedPoint] -> [FacetPoly] -> [FacetPoly] -> ([IndexedPoint], [FacetPoly])
createHorizontalUpFacets _ points newPoints newFacets [] =
  (newPoints, newFacets)
createHorizontalUpFacets y points newPoints newFacets (currentFacet:restFacets) =
  if isFacetForTransformingUp y points currentFacet
  then
    let freshPoints = map (\ip -> if (elem (index ip) currentFacet)
                                  then tryTransformPointUp y ip
                                  else ip) newPoints
    in if isFacetCrossingHorizontal y points currentFacet
       then
         let maxId = maximum $ map index newPoints
             id1 = maxId + 1
             id2 = maxId + 2
             [p1, p2, p3, p4] = (facetIdxToPoints points currentFacet)
             ([pa, pb], [pc, pd]) = splitByChecker [(p1, p2), (p2, p3), (p3, p4), (p4, p1)] (isEdgeCrossingHorizontal y)
             x1 = (px (dstvertex pa))
             x2 = (px (dstvertex pb))
             y1 = y
             y2 = y
             dy = abs (y - (py (dstvertex pd)))
             newp1 = IndexedPoint { index = id1
                                  , srcvertex = Point { px = (px (srcvertex pa)),
                                                        py = (py (srcvertex pd)) + signum ((py (srcvertex pa)) - (py (srcvertex pd))) * dy }
                                  , dstvertex = Point { px = x1, py = y1 } }
             newp2 = IndexedPoint { index = id2
                                  , srcvertex = Point { px = (px (srcvertex pc)),
                                                        py = (py (srcvertex pc)) + signum ((py (srcvertex pb)) - (py (srcvertex pc))) * dy }
                                  , dstvertex = Point { px = x2, py = y2 }}
             newFacet1 = [(index pa), (index pb), id2, id1]
             newFacet2 = [id1, id2, (index pc), (index pd)]
         in
           createHorizontalUpFacets y points (newp1 : newp2 : freshPoints) (newFacet1 : newFacet2 : newFacets) restFacets
       else
         createHorizontalUpFacets y points freshPoints (currentFacet : newFacets) restFacets
  else createHorizontalUpFacets y points newPoints (currentFacet : newFacets) restFacets

createVerticalRightFacets :: Number -> [IndexedPoint] -> [IndexedPoint] -> [FacetPoly] -> [FacetPoly] -> ([IndexedPoint], [FacetPoly])
createVerticalRightFacets _ points newPoints newFacets [] =
  (newPoints, newFacets)
createVerticalRightFacets x points newPoints newFacets (currentFacet:restFacets) =
  if isFacetForTransformingRight x points currentFacet
  then
    let freshPoints = map (\ip -> if (elem (index ip) currentFacet)
                                  then tryTransformPointRight x ip
                                  else ip) newPoints
    in if isFacetCrossingVertical x points currentFacet
       then
         let maxId = maximum $ map index newPoints
             id1 = maxId + 1
             id2 = maxId + 2
             [p1, p2, p3, p4] = (facetIdxToPoints points currentFacet)
             ([pa, pb], [pc, pd]) = splitByChecker [(p1, p2), (p2, p3), (p3, p4), (p4, p1)] (isEdgeCrossingVertical x)
             x1 = x
             x2 = x
             y1 = (py (dstvertex pa))
             y2 = (py (dstvertex pb))
             dx = abs (x - (px (dstvertex pd)))
             newp1 = IndexedPoint { index = id1
                                  , srcvertex = Point { px = (px (srcvertex pd)) + signum ((px (srcvertex pa)) - (px (srcvertex pd))) * dx,
                                                        py = (py (srcvertex pa)) }
                                  , dstvertex = Point { px = x1, py = y1 } }
             newp2 = IndexedPoint { index = id2
                                  , srcvertex = Point { px = (px (srcvertex pc)) + signum ((px (srcvertex pb)) - (px (srcvertex pc))) * dx,
                                                        py = (py (srcvertex pc)) }
                                  , dstvertex = Point { px = x2, py = y2 } }
             newFacet1 = [(index pa), (index pb), id2, id1]
             newFacet2 = [id1, id2, (index pc), (index pd)]
         in
           createVerticalRightFacets x points (newp1 : newp2 : freshPoints) (newFacet1 : newFacet2 : newFacets) restFacets
       else
         createVerticalRightFacets x points freshPoints (currentFacet : newFacets) restFacets
  else createVerticalRightFacets x points newPoints (currentFacet : newFacets) restFacets

createVerticalLeftFacets :: Number -> [IndexedPoint] -> [IndexedPoint] -> [FacetPoly] -> [FacetPoly] -> ([IndexedPoint], [FacetPoly])
createVerticalLeftFacets _ points newPoints newFacets [] =
  (newPoints, newFacets)
createVerticalLeftFacets x points newPoints newFacets (currentFacet:restFacets) =
  if isFacetForTransformingLeft x points currentFacet
  then
    let freshPoints = map (\ip -> if (elem (index ip) currentFacet)
                                  then tryTransformPointLeft x ip
                                  else ip) newPoints
    in if isFacetCrossingVertical x points currentFacet
       then
         let maxId = maximum $ map index newPoints
             id1 = maxId + 1
             id2 = maxId + 2
             [p1, p2, p3, p4] = (facetIdxToPoints points currentFacet)
             ([pa, pb], [pc, pd]) = splitByChecker [(p1, p2), (p2, p3), (p3, p4), (p4, p1)] (isEdgeCrossingVertical x)
             x1 = x
             x2 = x
             y1 = (py (dstvertex pa))
             y2 = (py (dstvertex pb))
             dx = abs (x - (px (dstvertex pd)))
             newp1 = IndexedPoint { index = id1
                                  , srcvertex = Point { px = (px (srcvertex pd)) + signum ((px (srcvertex pa)) - (px (srcvertex pd))) * dx,
                                                        py = (py (srcvertex pa)) }
                                  , dstvertex = Point { px = x1, py = y1 } }
             newp2 = IndexedPoint { index = id2
                                  , srcvertex = Point { px = (px (srcvertex pc)) + signum ((px (srcvertex pb)) - (px (srcvertex pc))) * dx,
                                                        py = (py (srcvertex pc)) }
                                  , dstvertex = Point { px = x2, py = y2 } }
             newFacet1 = [(index pa), (index pb), id2, id1]
             newFacet2 = [id1, id2, (index pc), (index pd)]
         in
           createVerticalLeftFacets x points (newp1 : newp2 : freshPoints) (newFacet1 : newFacet2 : newFacets) restFacets
       else
         createVerticalLeftFacets x points freshPoints (currentFacet : newFacets) restFacets
  else createVerticalLeftFacets x points newPoints (currentFacet : newFacets) restFacets

getReplacement :: [(PointIndex, PointIndex)] -> PointIndex -> PointIndex
getReplacement replacements value = snd $ fromJust (find (\(is, ir) -> is == value) replacements)

getReplacementPoint :: [(PointIndex, PointIndex)] -> IndexedPoint -> IndexedPoint
getReplacementPoint replacements ip =
  IndexedPoint { index = getReplacement replacements (index ip)
               , srcvertex = srcvertex ip
               , dstvertex = dstvertex ip }

normalizeSolutionDev :: [(PointIndex, [PointIndex])] -> Solution -> Solution
normalizeSolutionDev [] (Solution points facets) =
  let replacements = zip (sort $ map index points) [0..]
  in Solution { points = map (getReplacementPoint replacements) points
              , facets = map (\curFacet -> map (getReplacement replacements) curFacet) facets}
normalizeSolutionDev ((baseInd, []):restDuplicates) solution =
  normalizeSolutionDev restDuplicates solution
normalizeSolutionDev ((baseInd, (curDup:restDups)):restDuplicates) (Solution points facets) =
  let pointsWithoutDup = filter (\ip -> curDup /= (index ip)) points
      solutionWithoutDup = Solution { points = pointsWithoutDup
                                    , facets = map (\curFacet ->
                                                     map (\ind ->
                                                           if ind == curDup
                                                           then baseInd
                                                           else ind) curFacet) facets}
  in normalizeSolutionDev (((baseInd, restDups)):restDuplicates) solutionWithoutDup

normalizeSolution :: Solution -> Solution
normalizeSolution (Solution points facets) =
  let groupedPoints = groupBy (\pa pb -> (srcvertex pa) == (srcvertex pb)) $ sortOn srcvertex points
      duplicates = map (\l -> let (x:xs) = (sort l)
                              in (x, xs)) $ filter (\v -> (length v) > 1) $ map (map index) groupedPoints
  in normalizeSolutionDev duplicates (Solution { points = sortOn index points
                                               , facets = facets})

foldDown :: Number -> Solution -> Solution
foldDown y (Solution points facets) =
  let newObjects = createHorizontalDownFacets y points points [] facets
      newPoints = fst newObjects
      newFacets = snd newObjects
  in normalizeSolution Solution { points = newPoints
                                , facets = newFacets}

foldUp :: Number -> Solution -> Solution
foldUp y (Solution points facets) =
  let newObjects = createHorizontalUpFacets y points points [] facets
      newPoints = fst newObjects
      newFacets = snd newObjects
  in normalizeSolution Solution { points = newPoints
                                , facets = newFacets}

foldRight :: Number -> Solution -> Solution
foldRight x (Solution points facets) =
  let newObjects = createVerticalRightFacets x points points [] facets
      newPoints = fst newObjects
      newFacets = snd newObjects
  in normalizeSolution Solution { points = newPoints
                                , facets = newFacets}

foldLeft :: Number -> Solution -> Solution
foldLeft x (Solution points facets) =
  let newObjects = createVerticalLeftFacets x points points [] facets
      newPoints = fst newObjects
      newFacets = snd newObjects
  in normalizeSolution Solution { points = newPoints
                                , facets = newFacets}

reflectPointByEdge :: Edge -> Point -> Point
reflectPointByEdge ((Point x1 y1), (Point x2 y2)) =
  let
    dx = x2 - x1
    dy = y2 - y1
    n = dx
    m = dy
    b = dx * y1 - dy * x1
    pivot = Point { px = 0, py = b }
    toOrigin = translatePoint (negatePoint pivot)
    fromOrigin = translatePoint pivot
    reflect p = let k = 1 / (n * n + m * m)
                in Point { px = k * ((px p) * (1 - m * m) + (py p) * 2 *m)
                         , py = k * ((px p) * 2 * m + (py p) * (m * m - 1))
                         }
  in fromOrigin . reflect . toOrigin

foldByEdge :: Edge -> Solution -> Solution
foldByEdge edge solution =
    undefined
