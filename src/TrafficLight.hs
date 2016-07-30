module TrafficLight where

import Data.Maybe
import Data.List

data Side = West | North | East | South deriving (Eq, Show)

data Route = Route Side Side deriving (Eq, Show)

data Direction = Forward | TurnRight | TurnLeft deriving (Eq, Show)

data Move = Move Route Direction deriving (Eq, Show)

data Disposition = Parallel | Crossing deriving (Eq, Show)

direction :: Route -> Maybe Direction
direction (Route South East) = Just TurnRight
direction (Route South North) = Just Forward
direction (Route South West) = Just TurnLeft
direction (Route East North) = Just TurnRight
direction (Route East West) = Just Forward
direction (Route East South) = Just TurnLeft
direction (Route North West) = Just TurnRight
direction (Route North South) = Just Forward
direction (Route North East) = Just TurnLeft
direction (Route West South) = Just TurnRight
direction (Route West East) = Just Forward
direction (Route West North) = Just TurnLeft
direction _ = Nothing

disposition :: Side -> Side -> Disposition
disposition West North = Crossing
disposition West South = Crossing
disposition East North = Crossing
disposition East South = Crossing
disposition North West = Crossing
disposition North East = Crossing
disposition South West = Crossing
disposition South East = Crossing
disposition _ _ = Parallel

possibleRoutes :: [Route]
possibleRoutes = do
  src <- [West, North, East, South]
  dst <- [West, North, East, South]
  return $ Route src dst

routeDirection :: Route -> Maybe Move
routeDirection route = do
  dir <- direction route
  return $ Move route dir

possibleMoves :: [Move]
possibleMoves = catMaybes $ map routeDirection possibleRoutes

containsCrossing :: [Move] -> Bool
containsCrossing moves =
    any isCrossingPair pairs
        where
          pairs = filter ((==) 2 . length) $ subsequences moves
          isCrossingPair [a, b] = isCrossing a b

isCrossing :: Move -> Move -> Bool
isCrossing (Move (Route _ dstA) _) (Move (Route _ dstB) _) | dstA == dstB = True
isCrossing (Move (Route srcA _) dirA) (Move (Route srcB _) dirB) =
    case (disposition srcA srcB, dirA, dirB) of
      (Crossing, Forward, Forward) -> True
      (_, Forward, TurnLeft) | srcA /= srcB -> True
      (_, TurnLeft, Forward) | srcA /= srcB -> True
      (Crossing, TurnLeft, TurnLeft) -> True
      _ -> False

allowedConfigs :: [[Move]]
allowedConfigs =
    filter (not . containsCrossing) $ subsequences possibleMoves

printTraffic :: IO ()
printTraffic =
    mapM_ putStrLn $ map (show . routes) allowedConfigs
        where
          routes = map route
          route (Move r _) = r
