module SolutionDP where

import Data.List
import TrafficLight

type States = [Config]

initialState :: Config -> States
initialState cfg = [cfg]

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

solution :: Config -> Int
solution cfg =
    42
