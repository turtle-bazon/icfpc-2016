module BoundingBox where

import Data.Ratio
import Common
import Parse

bbox :: Poly -> (Point, Point)
bbox points =
    (topLeft, bottomRight)
        where
          topLeft = Point { px = minX, py = minY }
          bottomRight = Point { px = minX + side, py = minY + side }
          -- side = max (maxX - minX) (maxY - minY)
          side = 1 % 1
          minX = minimum $ map px points
          minY = minimum $ map py points
          maxX = maximum $ map px points
          maxY = maximum $ map py points

printSolution :: (Point, Point) -> IO ()
printSolution (tl, br) = do
  putStrLn "4"
  putStrLn "0,0"
  putStrLn "1,0"
  putStrLn "1,1"
  putStrLn "0,1"
  putStrLn "1"
  putStrLn "4 0 1 2 3"
  putStrLn $ showPoint $ Point (px tl) (py tl)
  putStrLn $ showPoint $ Point (px br) (py tl)
  putStrLn $ showPoint $ Point (px br) (py br)
  putStrLn $ showPoint $ Point (px tl) (py br)

parseFirstPoly :: String -> Poly
parseFirstPoly = polygon . head . filter isFillPoly . fst . parseSilhouette . lines
    where
      isFillPoly (PolyFill _) = True
      isFillPoly (PolyHole _) = False
