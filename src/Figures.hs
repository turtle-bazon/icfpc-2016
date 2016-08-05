module Figures where

import Data.Ratio
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
