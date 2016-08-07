module SolverRandomSearch (solverRandomSearch) where

import System.Random
import Common
import Math
import Figures
import SolverBBSimple
import Parse

centrifyPos :: Problem -> Point
centrifyPos = centrifyPoly . parseFirstPoly . silhouette

centrifyPoly :: Poly -> Point
centrifyPoly = center . bbox
    where
      center (Point { px = lbx, py = lby }, Point { px = rtx, py = rty }) =
          Point { px = lbx + (rtx - lbx) / 2, py = lby + (rty - lby) / 2 }

centrifySolution :: Solution -> Point
centrifySolution = centrifyPoly . dstPoly

dstPoly :: Solution -> Poly
dstPoly = map dstvertex . points

centrify :: Problem -> Solution -> Solution
centrify problem solution =
    translate relativeCenter solution
        where
          relativeCenter = translatePoint solutionDelta problemCenter
          problemCenter = centrifyPos problem
          solutionDelta = negatePoint $ centrifySolution solution

randomTranslationPoint :: Float -> IO Point
randomTranslationPoint variation = do
  deltaX <- randomRIO (-variation, variation)
  deltaY <- randomRIO (-variation, variation)
  return $ Point { px = approx deltaX, py = approx deltaY }

randomRotationAngle :: Float -> IO Float
randomRotationAngle variation = do
  delta <- randomRIO (-variation, variation)
  return $ pi * 2 * delta

randomAction :: Float -> IO (Solution -> Solution)
randomAction variation =
  randomRIO (0, 1) >>= choose
      where
        choose :: Int -> IO (Solution -> Solution)
        choose 0 = randomTranslationPoint variation >>= return . translate
        choose 1 = randomRotationAngle variation >>= return . rotateAroundCenter
        choose _ = error "shoud not get here"
        rotateAroundCenter angle solution =
            rotate (centrifySolution solution) angle solution

startSearch :: Silhouette -> Solution -> Int -> IO Solution
startSearch sil sol maxSteps =
    score sil sol >>= performSearch maxSteps sil sol

performSearch :: Int -> Silhouette -> Solution -> Double -> IO Solution
performSearch 0 _ sol curScore = stopSearch 0 sol curScore
performSearch stepsLeft sil sol curScore | curScore >= 1.0 = stopSearch stepsLeft sol curScore
performSearch stepsLeft sil sol curScore = do
  putStrLn $ " ;; Performing search with current score = " ++ (show curScore) ++ ", " ++ (show stepsLeft) ++ " steps left"
  tryAction <- randomAction 1.0
  let trySolution = tryAction sol
  tryScore <- score sil trySolution
  decideNext (tryScore / curScore) trySolution tryScore
      where
        decideNext alpha trySolution tryScore | alpha >= 1.0 = acceptNext trySolution tryScore
        decideNext alpha trySolution tryScore =
            do
              roll <- randomRIO (0.0, 1.0)
              rollNext alpha roll trySolution tryScore
        rollNext alpha roll | roll <= alpha = acceptNext
        rollNext _ _ = acceptCurr
        acceptNext = performSearch (stepsLeft - 1) sil
        acceptCurr _ _ = performSearch (stepsLeft - 1) sil sol curScore

stopSearch :: Int -> Solution -> Double -> IO Solution
stopSearch stepsLeft solution score = do
  putStrLn $ " ;; Searching stops with current score = " ++ (show score) ++ ", " ++ (show stepsLeft) ++ " steps left"
  return solution

solverRandomSearch :: Problem -> IO Solution
solverRandomSearch = solverRandomSearchSteps 100

solverRandomSearchSteps :: Int -> Problem -> IO Solution
solverRandomSearchSteps maxSteps problem =
    startSearch (silhouette problem) (centrify problem initSolution) maxSteps
