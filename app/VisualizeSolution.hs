module Main where

import Graphics.QML
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T

import Parse
import Visualize
import Text.JSON

import Paths_icfpc2016

main :: IO ()
main = do
    solutionData <- getContents
    solution <- return $ parseSolution $ lines solutionData
    model <- return $ encode $ makeSolutionModel solution
    state <- newIORef $ T.pack $ model
    -- putStrLn model
    clazz <- newClass [
      defPropertyConst' "modelJSON" (\_ -> readIORef state)
      ]
    ctx <- newObject clazz ()
    doc <- getDataFileName "solution.qml"
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument doc,
        contextObject = Just $ anyObjRef ctx}
