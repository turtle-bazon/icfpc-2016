module Figures where

import Data.List (nubBy)
import Data.Ratio
import Control.Monad (foldM)
import Algebra.Clipper
import Common
import Math

toIntPoint :: Point -> IntPoint
toIntPoint p =
    IntPoint x y
        where
          ox = (fromRational $ px p) * 1000000.0
          oy = (fromRational $ py p) * 1000000.0
          x = fromIntegral $ truncate ox
          y = fromIntegral $ truncate oy

fromIntPoint :: IntPoint -> Point
fromIntPoint (IntPoint x y) =
    Point { px = approx $ (fromIntegral x) / 1000000.0
          , py = approx $ (fromIntegral y) / 1000000.0
          }

intPointsEq :: IntPoint -> IntPoint -> Bool
intPointsEq (IntPoint xa ya) (IntPoint xb yb) | xa == xb && ya == yb = True
intPointsEq _ _ = False

toIntPoly :: Poly -> Polygon
toIntPoly = Polygon . map toIntPoint

fromIntPoly :: Polygon -> Poly
fromIntPoly (Polygon ps) =
    map fromIntPoint ps

makePolygons :: Point -> [Poly] -> Polygons
makePolygons pivot = Polygons . filter valid . map simplify . map toIntPoly . map toOrigin
    where
      toOrigin = map (translatePoint $ negatePoint pivot)
      simplify (Polygon pp) = Polygon $ nubBy intPointsEq pp
      valid (Polygon []) = False
      valid (Polygon [_]) = False
      valid (Polygon [_, _]) = False
      valid _ = True

choosePointLB :: [Poly] -> Point
choosePointLB = choosePointBy minimum

choosePointRT :: [Poly] -> Point
choosePointRT = choosePointBy maximum

choosePointBy :: ([Number] -> Number) -> [Poly] -> Point
choosePointBy choose polys =
    Point { px = getPoint px, py = getPoint py }
        where
          getPoint f = choose $ map f points
          points = concat polys

polyOp :: (Polygons -> Polygons -> IO Polygons) -> [Poly] -> [Poly] -> IO [Poly]
polyOp op psa psb = do
  let pivot = choosePointLB $ psa ++ psb
  Polygons ps <- op (makePolygons pivot psa) (makePolygons pivot psb)
  return $ map (map (translatePoint pivot)) $ map fromIntPoly ps

polyIntersect :: [Poly] -> [Poly] -> IO [Poly]
polyIntersect psa psb =
    calcIntersect
        where
          Point { px = lbxa, py = lbya } = choosePointLB psa
          Point { px = rtxa, py = rtya } = choosePointRT psa
          Point { px = lbxb, py = lbyb } = choosePointLB psb
          Point { px = rtxb, py = rtyb } = choosePointRT psb
          calcIntersect =
              case rtxa < lbxb || lbxa > rtxb || rtya < lbyb || lbya > rtyb of
                True -> return []
                False -> polyOp intersection psa psb

polyUnion :: [Poly] -> [Poly] -> IO [Poly]
polyUnion = polyOp union

polyDifference :: [Poly] -> [Poly] -> IO [Poly]
polyDifference = polyOp difference

polyArea :: Poly -> IO Double
polyArea p =
    calcArea
        where
          pivot = choosePointLB [p]
          calcArea = calc $ makePolygons pivot [p]
          calc (Polygons [poly]) = do
            areaE12 <- area poly
            return $ areaE12 / 1000000000000
          calc _ = return 0

silhouetteArea :: Silhouette -> IO Double
silhouetteArea sil = do
    areas <- sequence $ map area' sil
    return $ sum areas
        where
          area' (PolyFill poly) = polyArea poly
          area' (PolyHole poly) = do
                       a <- polyArea poly
                       return $ -a

silhouetteToPolygons :: Silhouette -> IO [Poly]
silhouetteToPolygons sil =
    traverse' sil []
        where
          traverse' [] ready = return $ ready
          traverse' ((PolyFill poly) : rest) ready = do
            merged <- polyUnion [poly] ready
            traverse' rest merged
          traverse' ((PolyHole poly) : rest) ready = do
            diffed <- polyDifference ready [poly]
            traverse' rest diffed

score :: Silhouette -> Solution -> IO Double
score sil sol = do
  facets <- foldM polyUnion [] $ map pure $ facetsPolys sol
  siPolys <- silhouetteToPolygons sil
  pIntersect <- polyIntersect facets siPolys
  verdict pIntersect facets siPolys
      where
        verdict [] _ _ = return 0
        verdict pIntersect facets siPolys =
            do
              pUnion <- polyUnion facets siPolys
              pIntersectAreas <- traverse polyArea pIntersect
              pUnionAreas <- traverse polyArea pUnion
              return $ (sum pIntersectAreas) / (sum pUnionAreas)

scoreRect :: Silhouette -> Point -> Point -> IO Double
scoreRect sil bottomLeft topRight = do
  rectSil <- return $ PolyFill [
    Point (px bottomLeft) (py bottomLeft),
    Point (px bottomLeft) (py topRight),
    Point (px topRight) (py topRight),
    Point (px topRight) (py bottomLeft)
    ]

  siPolys <- silhouetteToPolygons sil
  rectPolys <- silhouetteToPolygons [rectSil]
  pIntersect <- polyIntersect siPolys rectPolys
  verdict pIntersect siPolys rectPolys
      where
        verdict [] _ _ = return 0
        verdict pIntersect rectPolys siPolys =
            do
              pUnion <- polyUnion rectPolys siPolys
              pIntersectAreas <- traverse polyArea pIntersect
              pUnionAreas <- traverse polyArea pUnion
              return $ (sum pIntersectAreas) / (sum pUnionAreas)

isCongruentFacet :: [IndexedPoint] -> Bool
isCongruentFacet facet =
  let
    srcDist = multiDist (map srcvertex facet) []
    dstDist = multiDist (map dstvertex facet) []
  in
    foldl (\x (d1,d2) -> x && (d1 == d2)) True $ zip srcDist dstDist
  where
    multiDist [] res = res
    multiDist (p:[]) res = res
    multiDist (p1:p2:ps) res = multiDist (p2:ps) $ (dist p1 p2 : res)
    dist :: Point -> Point -> Number
    dist p1 p2 = (px p1 - px p2)*(px p1 - px p2) + (py p1 - py p2)*(py p1 - py p2)


isCongruentSolution :: Solution -> Bool
isCongruentSolution  = foldl (&&) True . map isCongruentFacet . facetsPolys'
