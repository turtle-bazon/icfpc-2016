module Main where

import System.Environment
import Graphics.QML
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T

import Parse
import Visualize
import SolversRun
import Text.JSON

import Paths_icfpc2016


main :: IO ()
main = do
  args <- getArgs
  processArgs args
      where
        processArgs (problemFile : solutionFile : _) = do
          problem <- loadProblem $ problemFile
          solution <- loadSolution $ solutionFile
          model <- return $ encode $ makeSolutionModel solution
          problemModel <- return $ encode $ makeProblemModel problem
          state <- newIORef $ T.pack $ model
          state2 <- newIORef $ T.pack $ problemModel
          -- putStrLn model
          clazz <- newClass [
            defPropertyConst' "modelJSON" (\_ -> readIORef state),
            defPropertyConst' "problemJSON" (\_ -> readIORef state2)
            ]
          ctx <- newObject clazz ()
          doc <- getDataFileName "solution.qml"
          runEngineLoop defaultEngineConfig {
            initialDocument = fileDocument doc,
            contextObject = Just $ anyObjRef ctx}
        processArgs _ = do
          processName <- getProgName
          putStrLn $ "Usage: " ++ processName ++ " <problem file> <solution file>"
