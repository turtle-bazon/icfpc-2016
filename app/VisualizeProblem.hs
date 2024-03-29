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
    problemData <- getContents
    problem <- return $ parseProblem $ lines problemData
    state <- newIORef $ T.pack $ encode $ makeProblemModel problem
    -- putStr $ encode problem
    clazz <- newClass [
      defPropertyConst' "problemJSON" (\_ -> readIORef state)
      ]
    ctx <- newObject clazz ()
    doc <- getDataFileName "problem.qml"
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument doc,
        contextObject = Just $ anyObjRef ctx}
