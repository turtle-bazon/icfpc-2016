module SolversRun where

import Data.List
import System.IO
import System.Directory
import Control.DeepSeq
import Common
import Parse
import Figures
import Show
import SolverBBSimple
import SolverBBRect
import SolverBBRotate

data Solver = Solver { name :: String, solve :: Problem -> Solution }

data Rating = Rating { solverName :: String, solution :: Solution, similarity :: Double } deriving (Show)

solvers :: [Solver]
solvers = [ Solver { name = "simple bbox", solve = solverBBSimple }
          , Solver { name = "rect bbox", solve = solverBBRect }
          , Solver { name = "rotate bbox", solve = solverBBRotate }
          ]

loadProblem :: String -> IO Problem
loadProblem filename = do
    fd <- openFile filename ReadMode
    contents <- hGetContents fd
    contents `deepseq` hClose fd
    return $ parseProblem $ lines contents

loadAllProblems :: String -> IO [(String, Problem)]
loadAllProblems dir = do
  files <- getDirectoryContents dir
  mapM (\f -> loadProblem (dir ++ "/" ++ f) >>= return . (,) f) $ filter (isSuffixOf ".txt") files

rateSolver :: Problem -> Solver -> IO Rating
rateSolver problem solver = do
  sim <- score (silhouette problem) solution
  return $ Rating { solverName = name solver
                  , solution = solution
                  , similarity = sim
                  }
      where
        solution = solve solver $ problem

rateSolvers :: Problem -> IO [Rating]
rateSolvers problem = mapM (rateSolver problem) solvers

bestSolver :: [Rating] -> Rating
bestSolver = head . reverse . sortOn similarity

runProblem :: String -> (String, Problem) -> IO ()
runProblem outputDir (problemFile, problem) = do
  ratings <- rateSolvers problem
  let best = bestSolver ratings
  let outFile = outputDir ++ "/" ++ problemFile
  fd <- openFile outFile WriteMode
  problemContents <- hPutStr fd $ showSolution $ solution best
  hClose fd
  putStrLn $ "Task " ++ problemFile ++ ", best score = " ++ (show $ similarity best) ++ " with solver: " ++ (solverName best)

run :: String -> String -> IO ()
run problemsDir outputDir = do
  problems <- loadAllProblems problemsDir
  mapM_ (runProblem outputDir) problems
