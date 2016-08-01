module Main where

import Data.Function (on)
import Data.List (find, sortBy)

data CrossRoads = CrossRoads {velocities :: [Int],
                              currentMinute :: Int,
                              minutes :: [CrossRoadsMinute]} deriving (Show)

data CrossRoadsMinute = CrossRoadsMinute {minute :: Int,
                                          cars :: [Int],
                                          traffic_lights :: [Int]} deriving (Show)

applyVelocity :: Int -> Int -> Int
applyVelocity car velocity = max 0 (car - velocity)

applyLights :: [Int] -> [Int] -> [Int] -> [Int]
applyLights lights velocities cars =
  zipWith applyVelocity cars $ zipWith (*) lights velocities

evaluateMinute :: CrossRoads -> [Int] -> CrossRoads
evaluateMinute cr tl =
  let now = currentMinute cr + 1
  in CrossRoads {velocities = velocities cr,
                 currentMinute = now,
                 minutes = CrossRoadsMinute {minute = now,
                                             cars = applyLights tl (velocities cr) (cars $ head $ minutes cr),
                                             traffic_lights = tl} : minutes cr}

isCrossing :: Int -> Int-> Bool
isCrossing routeA routeB =
  let workRouteA' = min routeA routeB
      workRouteB' = max routeA routeB
      base = div (workRouteA' - 1) 3
      workRouteA = workRouteA' - base * 3
      workRouteB = workRouteB' - base * 3
  in case workRouteA of
    1 -> elem workRouteB [9, 11] 
    2 -> elem workRouteB [4, 5, 6, 9, 11, 12]
    3 -> elem workRouteB [5, 6, 7, 8, 11, 12]

hasCrosses :: Int -> [Int] -> Bool
hasCrosses route lights =
  find (\lightNumber -> isCrossing route lightNumber) lights /= Nothing

chooseLightsDev :: [(Int, Int)] -> [Int] -> [Int]
chooseLightsDev [] lights = lights
chooseLightsDev cars lights =
  let (route, routeCars) = head cars
  in chooseLightsDev (tail cars)
     (if hasCrosses route lights
     then lights
     else (route : lights))

chooseLights :: [Int] -> [Int]
chooseLights cars =
  let sortedCars = sortBy ((flip  compare) `on` snd) $ zip [1..] cars
      devLights = chooseLightsDev sortedCars []
  in map (\lightNumber -> if (elem lightNumber devLights) then 1 else 0) [1..12]

parseList :: [Char] -> [Int]
parseList st =
  let replacer ' ' = ','
      replacer c = c
  in read $ "[" ++ map replacer st ++ "]"

parseTask :: [Char] -> [Char] -> CrossRoads
parseTask string_cars string_velocities =
  CrossRoads {velocities = parseList string_velocities,
              currentMinute = 0,
              minutes = [CrossRoadsMinute {minute = 0,
                                           cars = parseList string_cars,
                                           traffic_lights = map (const 0) [1..12]}]}

solve :: CrossRoads -> CrossRoads
solve cr
  | currentMinute cr == 10 = cr
  | otherwise = solve $ evaluateMinute cr $ chooseLights $ cars $ head $ minutes cr

main :: IO ()
main = do
  l1 <- getLine
  l2 <- getLine
  let cr = parseTask l1 l2 in
    print $ maximum $ cars $ head $ minutes $ solve cr
