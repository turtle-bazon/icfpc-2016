module Math where

import Debug.Trace
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

isUpper1Q :: Number -> Number -> Point -> Bool
isUpper1Q x y point =
  isPointLeft ( Point { px = x, py = y}
              , Point { px = x + 1, py = y + 1}) point

isUpper2Q :: Number -> Number -> Point -> Bool
isUpper2Q x y point =
  isPointLeft ( Point { px = x, py = y}
              , Point { px = x + 1, py = y - 1}) point

isUpper2QWeak :: Number -> Number -> Point -> Bool
isUpper2QWeak x y point =
  isPointLeftWeak ( Point { px = x, py = y}
                  , Point { px = x + 1, py = y - 1}) point

isLower2Q :: Number -> Number -> Point -> Bool
isLower2Q x y point =
  isPointRight ( Point { px = x, py = y}
               , Point { px = x + 1, py = y - 1}) point

isLower2QWeak :: Number -> Number -> Point -> Bool
isLower2QWeak x y point =
  isPointRightWeak ( Point { px = x, py = y}
                   , Point { px = x + 1, py = y - 1}) point

isUpper3Q :: Number -> Number -> Point -> Bool
isUpper3Q x y (Point px py) =
  (py - y) < (-px + x)

isUpper4Q :: Number -> Number -> Point -> Bool
isUpper4Q x y (Point px py) =
  (py - y) < (px - x)

isFacetForTransformingFold :: Edge -> [IndexedPoint] -> FacetPoly -> Bool
isFacetForTransformingFold edge points facet =
  let facetPoints = facetIdxToPoints points facet
      facetXYs = map dstvertex facetPoints
  in or $ map (isPointLeft edge) facetXYs

isFacetForTransformingTopLeftFold :: Number -> Number -> [IndexedPoint] -> FacetPoly -> Bool
isFacetForTransformingTopLeftFold x y points facet =
  let facetPoints = facetIdxToPoints points facet
      facetXYs = map dstvertex facetPoints
  in or $ map (isUpper1Q x y) facetXYs

isFacetForTransformingTopRightFold :: Number -> Number -> [IndexedPoint] -> FacetPoly -> Bool
isFacetForTransformingTopRightFold x y points facet =
  let facetPoints = facetIdxToPoints points facet
      facetXYs = map dstvertex facetPoints
  in or $ map (isUpper2Q x y) facetXYs

isFacetForTransformingBottomLeftFold :: Number -> Number -> [IndexedPoint] -> FacetPoly -> Bool
isFacetForTransformingBottomLeftFold x y points facet =
  let facetPoints = facetIdxToPoints points facet
      facetXYs = map dstvertex facetPoints
  in or $ map (isUpper3Q x y) facetXYs

isFacetForTransformingBottomRightFold :: Number -> Number -> [IndexedPoint] -> FacetPoly -> Bool
isFacetForTransformingBottomRightFold x y points facet =
  let facetPoints = facetIdxToPoints points facet
      facetXYs = map dstvertex facetPoints
  in or $ map (isUpper4Q x y) facetXYs

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

isEdgeCrossing :: Edge -> (IndexedPoint, IndexedPoint) -> Bool
isEdgeCrossing edge (p1, p2) =
  let facetXYs = map dstvertex [p1, p2] 
      hasPointsUpper = or $ map (isPointLeftWeak edge) facetXYs
      hasPointsLower = or $ map (isPointRight edge) facetXYs
  in hasPointsUpper && hasPointsLower

isEdgeCrossing1Q :: Number -> Number -> (IndexedPoint, IndexedPoint) -> Bool
isEdgeCrossing1Q x y (p1, p2) =
  let facetXYs = map dstvertex [p1, p2] 
      hasPointsUpper = or $ map (isUpper1Q x y) facetXYs
      hasPointsLower = or $ map (isUpper1Q x y) facetXYs
  in hasPointsUpper && hasPointsLower

isEdgeCrossing2Q :: Number -> Number -> (IndexedPoint, IndexedPoint) -> Bool
isEdgeCrossing2Q x y (p1, p2) =
  let facetXYs = map dstvertex [p1, p2] 
      hasPointsUpper = or $ map (isUpper2QWeak x y) facetXYs
      hasPointsLower = or $ map (isLower2Q x y) facetXYs
  in hasPointsUpper && hasPointsLower

isEdgeCrossing3Q = isEdgeCrossing1Q

isEdgeCrossing4Q = isEdgeCrossing2Q

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

isFacetCrossing :: Edge -> [IndexedPoint] -> FacetPoly -> Bool
isFacetCrossing edge points facet =
  let facetPoints = facetIdxToPoints points facet
      facetXYs = map dstvertex facetPoints
      hasPointsUpper = or $ map (isPointLeft edge) facetXYs
      hasPointsLower = or $ map (isPointRight edge) facetXYs
  in hasPointsUpper && hasPointsLower


isFacetCrossing1Q :: Number -> Number -> [IndexedPoint] -> FacetPoly -> Bool
isFacetCrossing1Q x y points facet =
  let facetPoints = facetIdxToPoints points facet
      facetXYs = map dstvertex facetPoints
      hasPointsUpper = or $ map (isUpper1Q x y) facetXYs
      hasPointsLower = or $ map (isUpper1Q x y) facetXYs
  in hasPointsUpper && hasPointsLower

isFacetCrossing2Q :: Number -> Number -> [IndexedPoint] -> FacetPoly -> Bool
isFacetCrossing2Q x y points facet =
  let facetPoints = facetIdxToPoints points facet
      facetXYs = map dstvertex facetPoints
      hasPointsUpper = or $ map (isUpper2Q x y) facetXYs
      hasPointsLower = or $ map (isLower2Q x y) facetXYs
  in hasPointsUpper && hasPointsLower

isFacetCrossing3Q = isFacetCrossing1Q

isFacetCrossing4Q = isFacetCrossing2Q

isFacetCrossingHorizontal :: Number -> [IndexedPoint] -> FacetPoly -> Bool
isFacetCrossingHorizontal y points facet =
  let [p1, p2, p3, p4] = (facetIdxToPoints points facet)
  in or $ map (isEdgeCrossingHorizontal y) [(p1, p2), (p2, p3), (p3, p4), (p4, p1)]

isFacetCrossingVertical :: Number -> [IndexedPoint] -> FacetPoly -> Bool
isFacetCrossingVertical x points facet =
  let [p1, p2, p3, p4] = (facetIdxToPoints points facet)
  in or $ map (isEdgeCrossingVertical x) [(p1, p2), (p2, p3), (p3, p4), (p4, p1)]

tryTransformPointFold :: Edge -> IndexedPoint -> IndexedPoint
tryTransformPointFold edge ip@(IndexedPoint idx srcv dstv) =
  if isPointLeft edge dstv
  then IndexedPoint { index = idx
                    , srcvertex = srcv
                    , dstvertex = reflectPointByEdge edge dstv }
  else ip

tryTransformPointTopLeftFold :: Number -> Number -> IndexedPoint -> IndexedPoint
tryTransformPointTopLeftFold x y (IndexedPoint idx srcv dstv) =
  if isUpper1Q x y dstv
  then IndexedPoint { index = idx
                    , srcvertex = srcv
                    , dstvertex = Point { px = 2 * x - (px dstv)
                                        , py = 2 * y - (py dstv)
                                        }}
  else IndexedPoint { index = idx
                    , srcvertex = srcv
                    , dstvertex = dstv }

tryTransformPointTopRightFold :: Number -> Number -> IndexedPoint -> IndexedPoint
tryTransformPointTopRightFold x y ip@(IndexedPoint idx srcv dstv) =
  if isUpper2Q x y dstv
  then IndexedPoint { index = idx
                    , srcvertex = srcv
                    , dstvertex = reflectPointByEdge foldingEdge dstv}
  else ip
  where foldingEdge = ( Point { px = x, py = y}
                      , Point { px = x + 1, py = y - 1})

tryTransformPointBottomRightFold :: Number -> Number -> IndexedPoint -> IndexedPoint
tryTransformPointBottomRightFold x y (IndexedPoint idx srcv dstv) =
  if isUpper3Q x y dstv
  then IndexedPoint { index = idx
                    , srcvertex = srcv
                    , dstvertex = Point { px = 2 * x - (px dstv)
                                        , py = 2 * y - (py dstv)
                                        }}
  else IndexedPoint { index = idx
                    , srcvertex = srcv
                    , dstvertex = dstv }

tryTransformPointBottomLeftFold :: Number -> Number -> IndexedPoint -> IndexedPoint
tryTransformPointBottomLeftFold x y (IndexedPoint idx srcv dstv) =
  if isUpper3Q x y dstv
  then IndexedPoint { index = idx
                    , srcvertex = srcv
                    , dstvertex = Point { px = 2 * x - (px dstv)
                                        , py = 2 * y - (py dstv)
                                        }}
  else IndexedPoint { index = idx
                    , srcvertex = srcv
                    , dstvertex = dstv }

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

toEdges :: [IndexedPoint] -> [(IndexedPoint, IndexedPoint)]
toEdges points@(firstPoint:restPoints) =
  zip points (restPoints ++ [firstPoint])

isPointLeft :: Edge -> Point-> Bool
isPointLeft ((Point x1 y1), (Point x2 y2)) (Point px py) =
  dy * (px - x1) < dx * (py - y1)
  where
    dx = x2 - x1
    dy = y2 - y1              

isPointLeftWeak :: Edge -> Point-> Bool
isPointLeftWeak ((Point x1 y1), (Point x2 y2)) (Point px py) =
  dy * (px - x1) <= dx * (py - y1)
  where
    dx = x2 - x1
    dy = y2 - y1              

isPointRight :: Edge -> Point-> Bool
isPointRight ((Point x1 y1), (Point x2 y2)) (Point px py) =
  dy * (px - x1) > dx * (py - y1)
  where
    dx = x2 - x1
    dy = y2 - y1              

isPointRightWeak :: Edge -> Point-> Bool
isPointRightWeak ((Point x1 y1), (Point x2 y2)) (Point px py) =
  dy * (px - x1) >= dx * (py - y1)
  where
    dx = x2 - x1
    dy = y2 - y1              

crossingPoint :: Edge -> Edge -> Maybe Point
crossingPoint ((Point x1_1 y1_1), (Point x2_1 y2_1)) ((Point x1_2 y1_2), (Point x2_2 y2_2)) =
  if (dcmbx == 0) || (dcmby == 0)
  then Nothing
  else Just Point { px = prex / dcmbx
                  , py = prey / dcmby
                  }
  where
    dx1 = x2_1 - x1_1
    dy1 = y2_1 - y1_1
    dx2 = x2_2 - x1_2
    dy2 = y2_2 - y1_2
    dcmbx = dx2 * dy1 - dx1 * dy2
    dcmby = dx1 * dy2 - dx2 * dy1
    prex = dx2 * dy1 * x1_1 - dx1 * dy2 * x1_2 - dx1 * dx2 * (y1_1 - y1_2)
    prey = dx1 * dy2 * y1_1 - dx2 * dy1 * y1_2 - dy1 * dy2 * (x1_1 - x1_2)

sourceByNeighbors :: IndexedPoint -> IndexedPoint -> IndexedPoint -> IndexedPoint
sourceByNeighbors p pa pb =
  IndexedPoint { index = index p
               , srcvertex = Point { px = (px (srcvertex pa)) + sdx * pcoeff
                                   , py = (py (srcvertex pa)) + sdy * pcoeff
                                   }
               , dstvertex = dstvertex p
               }
  where
    ddx = (px (dstvertex pb)) - (px (dstvertex pa))
    ddy = (py (dstvertex pb)) - (py (dstvertex pa))
    sdx = (px (srcvertex pb)) - (px (srcvertex pa))
    sdy = (py (srcvertex pb)) - (py (srcvertex pa))
    pdx = (px (dstvertex p)) - (px (dstvertex pa))
    pdy = (py (dstvertex p)) - (py (dstvertex pa))
    pcoeff = max (abs (if ddx == 0 then 0 else (pdx / ddx))) (abs (if ddy == 0 then 0 else (pdy / ddy)))

reduceEdgesToPoints :: [(IndexedPoint, IndexedPoint)] -> [IndexedPoint]
reduceEdgesToPoints [] = []
reduceEdgesToPoints ((p1, p2):restEdges) =
  p1:p2:(reduceEdgesToPoints restEdges)

separateEdgesBy :: ((IndexedPoint, IndexedPoint) -> Bool) -> [(IndexedPoint, IndexedPoint)] -> [[(IndexedPoint, IndexedPoint)]] -> Bool -> Bool -> Bool -> [[(IndexedPoint, IndexedPoint)]]
separateEdgesBy _ [] result _ _ _ = result
separateEdgesBy testfn (currentEdge:restEdges) result isCross isCrossSeen isFirstRun =
  if crossing
  then
    if isFirstRun
    then separateEdgesBy testfn (restEdges ++ [currentEdge]) [edgesBeforeCross, edgesCross, edgesAfterCross] False isCrossSeen True
    else
      if isCrossSeen
      then separateEdgesBy testfn restEdges [edgesBeforeCross, edgesCross ++ [currentEdge], edgesAfterCross] False isCrossSeen False
      else separateEdgesBy testfn restEdges [edgesBeforeCross, edgesCross ++ [currentEdge], edgesAfterCross] True True False
  else
    if isCross
    then separateEdgesBy testfn restEdges [edgesBeforeCross, edgesCross ++ [currentEdge], edgesAfterCross] isCross isCrossSeen False
    else
      if isCrossSeen
      then separateEdgesBy testfn restEdges [edgesBeforeCross, edgesCross, edgesAfterCross ++ [currentEdge]] isCross isCrossSeen False
      else separateEdgesBy testfn restEdges [edgesBeforeCross ++ [currentEdge], edgesCross, edgesAfterCross] isCross isCrossSeen False
  where
    crossing = (testfn currentEdge)
    edgesBeforeCross = head result
    edgesCross = head (tail result)
    edgesAfterCross = head (tail (tail result))

createAnyFoldFacets :: Edge -> [IndexedPoint] -> [IndexedPoint] -> [FacetPoly] -> [FacetPoly] -> ([IndexedPoint], [FacetPoly])
createAnyFoldFacets _ _ newPoints newFacets [] =
  (newPoints, newFacets)
createAnyFoldFacets foldingEdge points newPoints newFacets (currentFacet:restFacets) =
  if isFacetForTransformingFold foldingEdge points currentFacet
  then
    let freshPoints = map (\ip -> if (elem (index ip) currentFacet)
                                  then tryTransformPointFold foldingEdge ip
                                  else ip) newPoints
    in if isFacetCrossing foldingEdge points currentFacet
       then
         let maxId = maximum $ map index newPoints
             id1 = maxId + 1
             id2 = maxId + 2
             facetPoints = (facetIdxToPoints points currentFacet)
             facetEdges = toEdges facetPoints
             [edgesBefore, edgesCross, edgesAfter] = separateEdgesBy (isEdgeCrossing foldingEdge) facetEdges [[],[],[]] False False True
             se1 = edgesCross
             se2 = edgesAfter ++ edgesBefore
             e1 = head edgesCross
             e2 = last edgesCross
             e1_p1 = dstvertex $ fst e1
             e1_p2 = dstvertex $ snd e1
             e2_p1 = dstvertex $ fst e2
             e2_p2 = dstvertex $ snd e2
             crossp1 = fromJust (crossingPoint foldingEdge (e1_p1, e1_p2))
             crossp2 = fromJust (crossingPoint foldingEdge (e2_p1, e2_p2))
             newp1 = (sourceByNeighbors IndexedPoint { index = id1
                                                     , srcvertex = crossp1
                                                     , dstvertex = crossp1 } (fst e1) (snd e1))
             newp2 = (sourceByNeighbors IndexedPoint { index = id2
                                                     , srcvertex = crossp2
                                                     , dstvertex = crossp2} (fst e2) (snd e2))
             innerPoints' = nub $ reduceEdgesToPoints se1
             innerPoints = (init (tail innerPoints'))
             outerPoints = nub $ reduceEdgesToPoints se2
             newFacet1 = id2 : id1 : (map index innerPoints)
             newFacet2 = id1 : id2 : (map index outerPoints)
         in
           createAnyFoldFacets foldingEdge points (newp1 : newp2 : freshPoints) (newFacet1 : newFacet2 : newFacets) restFacets
       else
         createAnyFoldFacets foldingEdge points freshPoints (currentFacet : newFacets) restFacets
  else createAnyFoldFacets foldingEdge points newPoints (currentFacet : newFacets) restFacets

createTopRightFoldFacets :: Number -> Number -> [IndexedPoint] -> [IndexedPoint] -> [FacetPoly] -> [FacetPoly] -> ([IndexedPoint], [FacetPoly])
createTopRightFoldFacets _ _ _ newPoints newFacets [] =
  (newPoints, newFacets)
createTopRightFoldFacets x y points newPoints newFacets (currentFacet:restFacets) =
  if isFacetForTransformingTopRightFold x y points currentFacet
  then
    let freshPoints = map (\ip -> if (elem (index ip) currentFacet)
                                  then tryTransformPointTopRightFold x y ip
                                  else ip) newPoints
    in if isFacetCrossing2Q x y points currentFacet
       then
         let maxId = maximum $ map index newPoints
             id1 = maxId + 1
             id2 = maxId + 2
             foldingEdge = ( Point {px = x, py = y},
                             Point {px = x + 1, py = y - 1} )
             facetPoints = (facetIdxToPoints points currentFacet)
             facetEdges = toEdges facetPoints
             [edgesBefore, edgesCross, edgesAfter] = separateEdgesBy (isEdgeCrossing2Q x y) facetEdges [[],[],[]] False False True
             se1 = edgesCross
             se2 = edgesAfter ++ edgesBefore
             e1 = head edgesCross
             e2 = last edgesCross
             e1_p1 = dstvertex $ fst e1
             e1_p2 = dstvertex $ snd e1
             e2_p1 = dstvertex $ fst e2
             e2_p2 = dstvertex $ snd e2
             crossp1 = fromJust (crossingPoint foldingEdge (e1_p1, e1_p2))
             crossp2 = fromJust (crossingPoint foldingEdge (e2_p1, e2_p2))
             newp1 = (sourceByNeighbors IndexedPoint { index = id1
                                                     , srcvertex = crossp1
                                                     , dstvertex = crossp1 } (fst e1) (snd e1))
             newp2 = (sourceByNeighbors IndexedPoint { index = id2
                                                     , srcvertex = crossp2
                                                     , dstvertex = crossp2} (fst e2) (snd e2))
             innerPoints' = nub $ reduceEdgesToPoints se1
             innerPoints = (init (tail innerPoints'))
             outerPoints = nub $ reduceEdgesToPoints se2
             newFacet1 = id2 : id1 : (map index innerPoints)
             newFacet2 = id1 : id2 : (map index outerPoints)
         in
           createTopRightFoldFacets x y points (newp1 : newp2 : freshPoints) (newFacet1 : newFacet2 : newFacets) restFacets
       else
         createTopRightFoldFacets x y points freshPoints (currentFacet : newFacets) restFacets
  else createTopRightFoldFacets x y points newPoints (currentFacet : newFacets) restFacets

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
                                                     nub (map (\ind ->
                                                                if ind == curDup
                                                                then baseInd
                                                                else ind) curFacet)) facets}
  in normalizeSolutionDev (((baseInd, restDups)):restDuplicates) solutionWithoutDup

normalizeSolution :: Solution -> Solution
normalizeSolution (Solution points facets) =
  let groupedPoints = groupBy (\pa pb -> (srcvertex pa) == (srcvertex pb)) $ sortOn srcvertex points
      duplicates = map (\l -> let (x:xs) = (sort l)
                              in (x, xs)) $ filter (\v -> (length v) > 1) $ map (map index) groupedPoints
  in normalizeSolutionDev duplicates (Solution { points = sortOn index points
                                               , facets = facets})

foldAny :: Edge -> Solution -> Solution
foldAny foldingEdge (Solution points facets) =
  let newObjects = createAnyFoldFacets foldingEdge points points [] facets
      newPoints = fst newObjects
      newFacets = snd newObjects
  in normalizeSolution Solution { points = newPoints
                                , facets = newFacets}

foldTopRightCorner :: Number -> Number -> Solution -> Solution
foldTopRightCorner x y (Solution points facets) =
  let newObjects = createTopRightFoldFacets x y points points [] facets
      newPoints = fst newObjects
      newFacets = snd newObjects
  in normalizeSolution Solution { points = newPoints
                                , facets = newFacets}

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
