module TrafficLight where

import Data.Maybe
import Data.List

data Side = West | North | East | South deriving (Eq, Show)

data Route = Route Side Side deriving (Eq, Show)

data Direction = Forward | TurnRight | TurnLeft deriving (Eq, Show)

data Move = Move Route Direction deriving (Eq, Show)

data Disposition = Parallel | Crossing deriving (Eq, Show)

data Flow = Flow { route :: Route, amount :: Int, throughput :: Int } deriving (Eq, Show)

type Config = [Flow]

type GreenLights = [Route]

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
  (src, dst) <- [ (South, East)
                , (South, North)
                , (South, West)
                , (East, North)
                , (East, West)
                , (East, South)
                , (North, West)
                , (North, South)
                , (North, East)
                , (West, South)
                , (West, East)
                , (West, North)
                ]
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

allowedGreenLights :: [GreenLights]
allowedGreenLights =
    -- filter ((==) 4 . length) $ map routes $ filter (not . containsCrossing) $ subsequences possibleMoves
    map routes $ filter (not . containsCrossing) $ subsequences possibleMoves
        where
          routes = map route
          route (Move r _) = r

fromInput :: [(Int, Int)] -> Config
fromInput = zipWith makeFlow possibleMoves
    where makeFlow (Move route _) (amount, throughput) =
              Flow { route = route, amount = amount, throughput = throughput }

fromParsedInput :: [Int] -> [Int] -> Config
fromParsedInput amounts throughputs =
    fromInput $ zipWith (,) amounts throughputs

sampleInput :: Config
sampleInput =
    fromParsedInput [2, 0, 0, 14, 13, 0, 20, 0, 0, 0, 60, 7] [1, 1, 1, 1, 3, 1, 2, 1, 1, 1, 5, 1]

sampleInputErrA :: Config
sampleInputErrA =
    fromParsedInput [601, 782, 931, 916, 88, 51, 629, 36, 274, 838, 975, 193] [481, 764, 552, 204, 696, 416, 725, 272, 43, 123, 644, 268]

sampleInputErrB :: Config
sampleInputErrB =
    fromParsedInput [534, 865, 72, 56, 88, 659, 26, 351, 448, 316, 246, 744] [504, 110, 786, 898, 840, 582, 796, 284, 83, 673, 657, 427]

printTraffic :: IO ()
printTraffic =
    mapM_ putStrLn $ map show allowedGreenLights
