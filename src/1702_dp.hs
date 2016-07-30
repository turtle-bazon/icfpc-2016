module Main where

import Data.Maybe
import Data.List
import Data.Ord (comparing)

sortOn f = map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

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
    map routes $ filter (not . containsCrossing) $ subsequences possibleMoves
        where
          routes = map route
          route (Move r _) = r

fromInput :: [(Int, Int)] -> Config
fromInput = zipWith makeFlow possibleMoves
    where makeFlow (Move route _) (amount, throughput) =
              Flow { route = route, amount = amount, throughput = throughput }


data DpStep = DpStep { config :: Config, lights :: GreenLights, duration :: Int, cost :: Int } deriving (Eq, Show)

type DpTable = [DpStep]

initDpTable :: Config -> DpTable
initDpTable cfg = [DpStep { config = cfg, lights = [], duration = 0, cost = totalAmount cfg }]

totalAmount :: Config -> Int
totalAmount = sum . map amount

trafficRun :: Int -> GreenLights -> Config -> Config
trafficRun duration lights = map runLights
    where
      runLights flow | route flow `elem` lights = decr flow
      runLights flow = flow
      decr flow | amount flow < totalThroughput flow = flow { amount = 0 }
      decr flow = flow { amount = (amount flow) - (totalThroughput flow) }
      totalThroughput flow = duration * (throughput flow)

sortLights :: Config -> Int -> [GreenLights] -> [GreenLights]
sortLights cfg duration =
    map fst . sortOn rateLights . map runWithLights
        where
          runWithLights lights = (lights, trafficRun duration lights cfg)
          rateLights (_, cfg) = (slowestRouteFactor cfg, totalAmount cfg)

slowestRouteFactor :: Config -> Float
slowestRouteFactor =
    head . reverse . sort . map pseudoFactor
        where
          pseudoFactor flow | factor flow < 1.0 = 0
          pseudoFactor flow = factor flow
          factor flow = (fromIntegral $ amount flow) / (fromIntegral $ throughput flow)

solutionStep :: DpTable -> Int -> DpTable
solutionStep dpTable maxDuration =
    dpTable ++ [bestDpStep]
        where
          possibleSteps = map step backSteps
          backSteps = reverse [1 .. maxDuration]
          step duration = mkDpStep duration (prevCfg duration) $ head $ sortLights (prevCfg duration) duration allowedGreenLights
          mkDpStep duration cfg lights =
              DpStep { config = trafficRun duration lights cfg
                     , lights = lights
                     , duration = duration
                     , cost = totalAmount $ trafficRun duration lights cfg
                     }
          prevCfg duration = config $ dpTable !! (maxDuration - duration)
          dpSteps = possibleSteps
          bestDpStep = head $ sortOn (totalAmount . config) dpSteps

solutionDp :: Config -> DpTable
solutionDp cfg =
    foldl solutionStep init durations
        where
          durations = [1 .. 10]
          init = initDpTable cfg

answerDp :: DpTable -> Int
answerDp = head . reverse . sort . map amount . config . last

parseInput :: String -> [(Int, Int)]
parseInput contents =
    zipWith (,) (numbers !! 0) (numbers !! 1)
        where
          numbers = map parseNums $ lines contents
          parseNums :: String -> [Int]
          parseNums = map read . words

main :: IO ()
main = do
  contents <- getContents
  putStrLn (show $ answerDp $ solutionDp $ fromInput $ parseInput contents)
