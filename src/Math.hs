module Math where

import Data.List
import Data.Maybe
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

translate :: Point -> Solution -> Solution
translate delta solution =
    solution { points = map translateDst $ points solution }
        where
          translateDst p = p { dstvertex = translatePoint delta $ dstvertex p }

rotate :: Point -> Float -> Solution -> Solution
rotate pivot angle solution =
    solution { points = map rotateDst $ points solution }
        where
          rotateDst p = p { dstvertex = rotatePoint pivot angle $ dstvertex p }

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

normalizeSolutionDev :: [(PointIndex, [PointIndex])] -> Solution -> Solution
normalizeSolutionDev [] solution =
  solution
normalizeSolutionDev ((baseInd, []):restDuplicates) solution =
  normalizeSolutionDev restDuplicates solution
normalizeSolutionDev ((baseInd, (curDup:restDups)):restDuplicates) (Solution points facets) =
  let pointsWithoutDup = filter (\ip -> curDup /= (index ip)) points
      replacements = (curDup, baseInd) : (map (\ind -> if ind > curDup then (ind, ind - 1) else (ind, ind)) $ map index pointsWithoutDup)
      solutionWithoutDup = Solution { points = map (\ip -> let curInd = (index ip)
                                                               repInd = snd $ fromJust (find (\(is, ir) -> is == curInd) replacements)
                                                           in IndexedPoint { index = repInd
                                                                           , srcvertex = srcvertex ip
                                                                           , dstvertex = dstvertex ip}) pointsWithoutDup
                                    , facets = map (\curFacet -> map (\ind -> snd $ fromJust (find (\(is, ir) -> is == ind) replacements)) curFacet) facets}
  in normalizeSolutionDev (((baseInd, restDups)):restDuplicates) solutionWithoutDup

normalizeSolution :: Solution -> Solution
normalizeSolution (Solution points facets) =
  let groupedPoints = groupBy (\pa pb -> (srcvertex pa) == (srcvertex pb)) $ sortBy (\pa pb -> compare (srcvertex pa) (srcvertex pb)) points
      duplicates = map (\l -> let (x:xs) = (sort l)
                              in (x, xs)) $ filter (\v -> (length v) > 1) $ map (\plist -> (map (\ip -> (index ip)) plist)) groupedPoints
  in normalizeSolutionDev duplicates (Solution { points = sortBy (\ip1 ip2 -> compare (index ip1) (index ip2)) points
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

foldByEdge :: Edge -> Solution -> Solution
foldByEdge edge solution =
    undefined
