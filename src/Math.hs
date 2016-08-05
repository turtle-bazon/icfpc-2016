module Math where

import Debug.Trace
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
    Point { px = -(px point), py = -(px point) }

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

fold :: Edge -> (Number -> Number -> Bool) -> Solution -> Solution
fold edge sideComparator solution =
  let line = lineForEdge edge
  in Solution {points = points solution
              , facets = facets solution
               --dst = map (\p -> if isPointPosition p line sideComparator
               --                 then reflectPointBy p line
               --                 else p) $ dst solution
              }

facetIdxToPoints :: [IndexedPoint] -> FacetPoly -> [IndexedPoint]
facetIdxToPoints points facet =
  map (\ind -> fromJust (find ((== ind) . index) points)) facet

isFacetForTransformingDown :: Number -> [IndexedPoint] -> FacetPoly -> Bool
isFacetForTransformingDown y points facet =
  let facetPoints = facetIdxToPoints points facet
      facetYs = (map (\ip -> py $ dstvertex ip) facetPoints)
  in y < (maximum facetYs)

isEdgeCrossingHorizontal :: Number -> (IndexedPoint, IndexedPoint) -> Bool
isEdgeCrossingHorizontal y (p1, p2) =
  let maxy = max (py (dstvertex p1)) (py (dstvertex p2))
      miny = min (py (dstvertex p1)) (py (dstvertex p2))
  in (y > miny) && (y < maxy)

isFacetCrossingHorizontal :: Number -> [IndexedPoint] -> FacetPoly -> Bool
isFacetCrossingHorizontal y points facet =
  let [p1, p2, p3, p4] = (facetIdxToPoints points facet)
  in or $ map (isEdgeCrossingHorizontal y) [(p1, p2), (p2, p3), (p3, p4), (p4, p1)]

tryTransformPointDown :: Number -> IndexedPoint -> IndexedPoint
tryTransformPointDown y (IndexedPoint idx srcv dstv) =
  if y < (py dstv)
  then IndexedPoint { index = idx
                    , srcvertex = srcv
                    , dstvertex = Point { px = px dstv, py = 2 * y - (py dstv) }}
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
                                  , srcvertex = Point { px = (px (srcvertex pb)),
                                                        py = (py (srcvertex pb)) + signum ((py (srcvertex pc)) - (py (srcvertex pb))) * dy }
                                  , dstvertex = Point { px = x2, py = y2 }}
             newFacet1 = [(index pa), (index pb), id2, id1]
             newFacet2 = [id1, id2, (index pc), (index pd)]
         in
           createHorizontalDownFacets y points (newp1 : newp2 : freshPoints) (newFacet1 : newFacet2 : newFacets) restFacets
       else           
         createHorizontalDownFacets y points freshPoints (currentFacet : newFacets) restFacets
  else createHorizontalDownFacets y points newPoints (currentFacet : newFacets) restFacets

foldDown :: Number -> Solution -> Solution
foldDown y (Solution points facets) =
  let newObjects = createHorizontalDownFacets y points points [] facets
      newPoints = fst newObjects
      newFacets = snd newObjects
  in Solution { points = newPoints
              , facets = newFacets}

foldUp :: Number -> Solution -> Solution
foldUp y (Solution points facets) =
  Solution { points = points
           , facets = facets}

foldRight :: Number -> Solution -> Solution
foldRight x (Solution points facets) =
  Solution { points = points
           , facets = facets}

foldLeft :: Number -> Solution -> Solution
foldLeft x (Solution points facets) =
  Solution { points = points
           , facets = facets}

