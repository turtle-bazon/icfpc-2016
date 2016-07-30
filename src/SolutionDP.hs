module SolutionDP where

import TrafficLight

type States = [Config]

initialState :: Config -> States
initialState cfg = [cfg]

totalAmount :: Config -> Int
totalAmount = sum . map amount

minuteRun :: GreenLights -> Config -> Config
minuteRun lights = map runLights
    where
      runLights flow | route flow `elem` lights = flow { amount = (amount flow) - (throughput flow) }
      runLights flow = flow

solution :: Config -> Int
solution cfg =
    42
