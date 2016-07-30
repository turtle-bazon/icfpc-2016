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
              let nextConfig = trafficRun duration lights cfg
              in DpStep { config = nextConfig
                        , lights = lights
                        , duration = duration
                        , cost = totalAmount $ nextConfig
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

ppSolution :: Config -> IO ()
ppSolution cfg = do
  putStrLn $ "Answer is: " ++ (show $ answerDp dpTable)
  putStrLn $ "Total cars waiting at start: " ++ (show $ totalAmount cfg)
  mapM_ ppStep actualSteps
    where
      ppStep step =
          putStrLn $ "For "
                       ++ (show $ duration step)
                       ++ " minutes green lights for: "
                       ++ (show $ lights step)
                       ++ ", now cars waiting: "
                       ++ (show $ cost step)
      actualSteps = collectSteps 0 []
      dpTable = solutionDp cfg
      dpTableSize = (length dpTable) - 1
      collectSteps minutes acc | minutes == dpTableSize = acc
      collectSteps minutes acc =
          let
              index = dpTableSize - minutes
              step = dpTable !! index
              spend = minutes + (duration step)
          in
            collectSteps spend $ step : acc
