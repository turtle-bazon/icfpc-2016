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
import SolverMetropolis
import SolverMinimalRect
import SolverMultiRect

data Solver = Solver { name :: String, solve :: Problem -> IO Solution }

data Rating = Rating { solverName :: String, solution :: Solution, similarity :: Double } deriving (Show)

solvers :: [Solver]
solvers = [ 
            Solver { name = "simple", solve = solverBBSimple }
          , Solver { name = "rect", solve = solverBBRect }
          , Solver { name = "rotate", solve = solverBBRotate }
          , Solver { name = "metropolis", solve = solverMetropolis }          
          , Solver { name = "multirect", solve = solverMultiRect }
          -- , Solver { name = "minirect", solve = solverMinimalRect }
          ]

loadFile :: ( [String] -> a ) -> String -> IO a
loadFile parser filename = do
    fd <- openFile filename ReadMode
    contents <- hGetContents fd
    contents `deepseq` hClose fd
    return $ parser $ lines contents

loadProblem :: String -> IO Problem
loadProblem = loadFile parseProblem

loadSolution :: String -> IO Solution
loadSolution = loadFile parseSolution

loadAllProblems :: String -> IO [(String, Problem)]
loadAllProblems dir = do
  files <- getDirectoryContents dir
  mapM (\f -> loadProblem (dir ++ "/" ++ f) >>= return . (,) f) $ filter (isSuffixOf ".txt") files

rateSolver :: Problem -> Solver -> IO Rating
rateSolver problem solver = do
  solution <- solve solver $ problem
  sim <- score (silhouette problem) solution
  return $ Rating { solverName = name solver
                  , solution = solution
                  , similarity = sim
                  }

rateSolvers :: Problem -> IO Rating
rateSolvers problem =
    inspectSolvers solvers Nothing
        where
          inspectSolvers _ (Just rating@(Rating { similarity = sim })) | sim >= 1.0 = return rating
          inspectSolvers [] (Just rating) = return rating
          inspectSolvers (solver : restSolvers) curRating = do
            newRating <- rateSolver problem solver
            inspectNext restSolvers curRating newRating
          inspectNext restSolvers rating (Rating { solution = sol })
              | solutionLength sol > 5000 = inspectSolvers restSolvers rating
          inspectNext restSolvers Nothing rating = inspectSolvers restSolvers $ Just rating
          inspectNext restSolvers curRating@(Just (Rating { similarity = curSim })) newRating@(Rating { similarity = newSim })
              | newSim > curSim = inspectSolvers restSolvers $ Just newRating
          inspectNext restSolvers rating _ = inspectSolvers restSolvers rating

runProblem :: String -> (String, Problem) -> IO ()
runProblem outputDir (problemFile, problem) = do
  best <- rateSolvers problem
  let outFile = outputDir ++ "/" ++ problemFile
  fd <- openFile outFile WriteMode
  problemContents <- hPutStr fd $ showSolution $ solution best
  hClose fd
  putStrLn $ "Task " ++ problemFile ++ ", best score = " ++ (show $ similarity best) ++ " with solver: " ++ (solverName best)

run :: String -> String -> IO ()
run problemsDir outputDir = do
  problems <- loadAllProblems problemsDir
  mapM_ (runProblem outputDir) problems

runSingle :: String -> String -> IO ()
runSingle solverName problemFile = do
  problem <- loadProblem problemFile
  runSingleSolver problem $ filter (\Solver { name = name } -> name == solverName) solvers
    where
      runSingleSolver problem ( solver : _ ) = do
        rating <- rateSolver problem solver
        putStrLn $ showSolution $ solution rating
        putStrLn $ "Score = " ++ (show $ similarity rating);
      runSingleSolver _ _  = putStrLn $ "No solver named '" ++ solverName ++ "' found"
