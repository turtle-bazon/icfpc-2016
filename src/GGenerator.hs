module GGenerator where

import Data.Ratio
import Common
import Math
import Parse
import Show


enumeratePoints :: [Point] -> [IndexedPoint]
enumeratePoints points = 
	map (\(i,v) -> IndexedPoint { index = i, vertex = v }) $ zip [0..(length points - 1)] points

-- foldG n w 
-- generates symbol Г using line with thinckness 1/n and bend on distance w from end point
-- n should be in range [2;+INF]. Bigger n => thinner line => bigger solution file size in bytes. 
-- w should be in range (0;1).

-- USAGE EXAMPLE: ghci> putStrLn $ showSolution $ rotate (Point 0 0) pi/17 $ foldG 20 (1%11)

foldG :: Integer -> Number -> Solution
foldG n w =
    let
        h = 1 % n
        as = [ Point 0 (i % n) | i <- [0..n] ]
        bs = [ Point 1 (i % n) | i <- [0..n] ]        
        cs = [if even i then Point w (i%n) else Point (w+h) (i%n) | i <- [0..n]]

        as' = [if even i then Point 0 0 else Point 0 h | i <- [0..n]]
        bs' = [if even i then Point w (1-w) else Point (w+h) (1-w) | i <- [0..n]]
        cs' = [if even i then Point w 0 else Point (w+h) h | i <- [0..n]]

        acb = reverse $ foldl (\acc -> \(a,b,c) -> b:c:a:acc ) [] $ zip3 as bs cs
        acb' = reverse $ foldl (\acc -> \(a,b,c) -> b:c:a:acc ) [] $ zip3 as' bs' cs'

        facetsA = [ map fromInteger [3*i+0, 3*i+1, 3*i+4, 3*i+3] | i <- [0..n-1]]
        facetsB = [ map fromInteger [3*i+1, 3*i+2, 3*i+5, 3*i+4] | i <- [0..n-1]]

    in
        Solution { src = enumeratePoints acb,
                   facets = facetsA ++ facetsB,
                   dst = acb' }


zip4 [] _ _ _ = []
zip4 _ [] _ _ = []
zip4 _ _ [] _ = []
zip4 _ _ _ [] = []
zip4 (a:as) (b:bs) (c:cs) (d:ds) =
	(a,b,c,d) : (zip4 as bs cs ds)


foldS :: Integer -> Number -> Number -> Solution
foldS n w1 w2 =
    let
        h = 1 % n
        as = [ Point 0 (i % n) | i <- [0..n] ]
        bs = [ Point 1 (i % n) | i <- [0..n] ]        
        w1s = [if even i then Point w1 (i%n) else Point (w1+h) (i%n) | i <- [0..n]]
        w2s = [if even i then Point (w1+w2) (i%n) else Point (w1+w2+h) (i%n) | i <- [0..n]]

        as' = [if even i then Point 0 0 else Point 0 h | i <- [0..n]]
        bs' = [if even i then Point (1-w2) w2 else Point (1-w2) (w2+h) | i <- [0..n]]
        w1s' = [if even i then Point w1 0 else Point (w1+h) h | i <- [0..n]]
        w2s' = [if even i then Point w1 w2 else Point (w1+h) (w2+h) | i <- [0..n]]

        acdb = reverse $ foldl (\acc -> \(a,b,c,d) -> b:d:c:a:acc ) [] $ zip4 as bs w1s w2s
        acdb' = reverse $ foldl (\acc -> \(a,b,c,d) -> b:d:c:a:acc ) [] $ zip4 as' bs' w1s' w2s'

        facetsA = [ map fromInteger [4*i+0, 4*i+1, 4*i+5, 4*i+4] | i <- [0..n-1]]
        facetsB = [ map fromInteger [4*i+1, 4*i+2, 4*i+6, 4*i+5] | i <- [0..n-1]]
        facetsC = [ map fromInteger [4*i+2, 4*i+3, 4*i+7, 4*i+6] | i <- [0..n-1]]

    in
        Solution { src = enumeratePoints acdb,
                   facets = facetsA ++ facetsB ++ facetsC,
                   dst = acdb' }

generateG :: Integer -> Number -> Float -> Solution
generateG n w angle =
	rotate (Point (1%2) (1%2)) angle $ foldG n w