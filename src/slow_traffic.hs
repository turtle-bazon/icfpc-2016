module Main where

import Data.Bits (testBit)
import Data.Function (on)
import Data.List (elemIndex, find, minimumBy, sortBy)
import System.Environment (getArgs)

data CrossRoads = CrossRoads {velocities :: [Int],
                              variant :: Int,
                              currentMinute :: Int,
                              minutes :: [CrossRoadsMinute]} deriving (Show)

data CrossRoadsMinute = CrossRoadsMinute {minute :: Int,
                                          cars :: [Int],
                                          traffic_lights :: [Int],
                                          debug_lights :: [Int]} deriving (Show)

applyVelocity :: Int -> Int -> Int
applyVelocity car velocity = max 0 (car - velocity)

applyLights :: [Int] -> [Int] -> [Int] -> [Int]
applyLights lights velocities cars =
  zipWith applyVelocity cars $ zipWith (*) lights velocities

evaluateMinute :: CrossRoads -> ([Int], [Int]) -> CrossRoads
evaluateMinute cr (tl, dl) =
  let now = currentMinute cr + 1
  in CrossRoads {velocities = velocities cr,
                 variant = variant cr,
                 currentMinute = now,
                 minutes = CrossRoadsMinute {minute = now,
                                             cars = applyLights tl (velocities cr) (cars $ head $ minutes cr),
                                             traffic_lights = tl,
                                             debug_lights = dl} : minutes cr}

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

isRightTurn :: Int -> Bool
isRightTurn lightNumber = elem lightNumber [1, 4, 7, 10]

isStraight :: Int -> Bool
isStraight lightNumber = elem lightNumber [2, 5, 8, 11]

isLeftTurn :: Int -> Bool
isLeftTurn lightNumber = elem lightNumber [3, 6, 9, 12]

lightCompareAll :: (Int, Int) -> (Int, Int) -> Ordering
lightCompareAll (lightNumber1, cars1) (lightNumber2, cars2)
  | cars1 == 0 = LT
  | cars2 == 0 = GT
  | otherwise = compare cars1 cars2

lightCompareExceptTurnRight :: (Int, Int) -> (Int, Int) -> Ordering
lightCompareExceptTurnRight (lightNumber1, cars1) (lightNumber2, cars2)
  | isRightTurn lightNumber1 && not (isRightTurn lightNumber2) = LT
  | not (isRightTurn lightNumber1) && isRightTurn lightNumber2 = GT
  | otherwise = lightCompareAll (lightNumber1, cars1) (lightNumber2, cars2)

lightCompare :: (Int, Int) -> (Int, Int) -> Ordering
lightCompare (lightNumber1, cars1) (lightNumber2, cars2)
  | cars1 == 0 = LT
  | cars2 == 0 = GT
--  | isRightTurn lightNumber1 && not (isRightTurn lightNumber2) = LT
--  | not (isRightTurn lightNumber1) && isRightTurn lightNumber2 = GT
--  | isLeftTurn lightNumber1 && not (isLeftTurn lightNumber2) = GT
--  | not (isLeftTurn lightNumber1) && isLeftTurn lightNumber2 = LT
  | otherwise = compare cars1 cars2

compareLight :: Int -> Int -> Ordering
compareLight l1 l2 =
  EQ

lightComparePriority :: (Int, Int) -> (Int, Int) -> Ordering
lightComparePriority (lightNumber1, cars1) (lightNumber2, cars2) =
  EQ

chooseLights :: Int -> Int -> [Int] -> ([Int], [Int])
chooseLights variant currentMinute cars =
  let --lightCompare = if (testBit variant (currentMinute - 1)) then lightCompareExceptTurnRight else lightCompareAll
      sortedCars = sortBy (flip lightCompare) $ zip [1..] cars
      devLights = chooseLightsDev sortedCars []
  in (map (\lightNumber -> if (elem lightNumber devLights) then 1 else 0) [1..12], sortBy compare devLights)

parseList :: [Char] -> [Int]
parseList st =
  let replacer ' ' = ','
      replacer c = c
  in read $ "[" ++ map replacer st ++ "]"

parseTask :: [Char] -> [Char] -> CrossRoads
parseTask string_cars string_velocities =
  CrossRoads {velocities = parseList string_velocities,
              variant = 0,
              currentMinute = 0,
              minutes = [CrossRoadsMinute {minute = 0,
                                           cars = parseList string_cars,
                                           traffic_lights = map (const 0) [1..12],
                                           debug_lights = []}]}

solve :: CrossRoads -> CrossRoads
solve cr
  | currentMinute cr == 2 = cr
  | otherwise = solve $ evaluateMinute cr $ chooseLights (variant cr) (currentMinute cr) (cars $ head $ minutes cr)

constructVariant :: (CrossRoads, Int) -> CrossRoads
constructVariant (cr, variant) =
  CrossRoads {velocities = velocities cr,
              variant = variant,
              currentMinute = 0,
              minutes = minutes cr}

compareSolutions :: CrossRoads -> CrossRoads -> Ordering
compareSolutions cr1 cr2 =
  compare (maximum $ cars $ head $ minutes $ cr1) (maximum $ cars $ head $ minutes $ cr2)

main :: IO ()
main = do
  cmdArgs <- getArgs
  l1 <- getLine
  l2 <- getLine
  let cr = parseTask l1 l2
      cr_variants = map constructVariant $ zip (repeat cr) [0..0]
      cr_solutions = map solve cr_variants
      best_solution = minimumBy compareSolutions cr_solutions
    in
    case cmdArgs of
      ["print"] -> print best_solution
      [] -> print $ maximum $ cars $ head $ minutes best_solution
