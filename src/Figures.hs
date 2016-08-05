module Figures where

import Data.Ratio
import Algebra.Clipper
import Common
import Math
import Parse

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

toIntPoly :: Poly -> Polygon
toIntPoly = Polygon . map toIntPoint

fromIntPoly :: Polygon -> Poly
fromIntPoly (Polygon ps) =
    map fromIntPoint ps

polyOp :: (Polygons -> Polygons -> IO Polygons) -> [Poly] -> [Poly] -> IO [Poly]
polyOp op psa psb = do
    let ipsa = Polygons $ map toIntPoly psa
    let ipsb = Polygons $ map toIntPoly psb
    Polygons ps <- op ipsa ipsb
    return $ map fromIntPoly ps

polyIntersect :: [Poly] -> [Poly] -> IO [Poly]
polyIntersect = polyOp intersection

polyUnion :: [Poly] -> [Poly] -> IO [Poly]
polyUnion = polyOp union

polyDifference :: [Poly] -> [Poly] -> IO [Poly]
polyDifference = polyOp difference

polyArea :: Poly -> IO Double
polyArea p = do
  let poly = toIntPoly p
  areaE12 <- area poly
  return $ areaE12 / 1000000000000

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

score :: Silhouette -> [Poly] -> IO Double
score sil facets = do
  siPolys <- silhouetteToPolygons sil
  pIntersect <- polyIntersect facets siPolys
  pUnion <- polyUnion facets siPolys
  pIntersectAreas <- traverse polyArea pIntersect
  pUnionAreas <- traverse polyArea pUnion
  return $ (sum pIntersectAreas) / (sum pUnionAreas)
