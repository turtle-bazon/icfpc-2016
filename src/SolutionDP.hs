module SolutionDP where

import Data.List
import TrafficLight

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
          rateLights (_, cfg) = totalAmount cfg

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

solution :: Config -> Int
solution = answerDp . solutionDp
