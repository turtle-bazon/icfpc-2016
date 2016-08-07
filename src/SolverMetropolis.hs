module SolverMetropolis (solverMetropolis) where

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

data Trans = Trans { baseSolution :: Solution
                   , foldHistory :: [Solution -> Solution]
                   , transHistory :: [Solution -> Solution]
                   }

makeTrans :: Solution -> Trans
makeTrans sol =
    Trans { baseSolution = sol, foldHistory = [], transHistory = [] }

play :: Trans -> Solution
play trans =
    foldr run (baseSolution trans) $ (foldHistory trans) ++ (transHistory trans)
        where
          run f solution = f solution

randomAction :: Float -> Trans -> IO Trans
randomAction variation tr =
  randomRIO (0, 1) >>= choose
      where
        choose :: Int -> IO Trans
        choose 0 = do
          point <- randomTranslationPoint variation
          return $ tr { transHistory = (translate point) : (transHistory tr) }
        choose 1 = do
          angle <- randomRotationAngle variation
          return $ tr { transHistory = (rotateAroundCenter angle) : (transHistory tr) }
        choose _ = error "shoud not get here"
        rotateAroundCenter angle solution =
            rotate (centrifySolution solution) angle solution

data Step = Step { trans :: Trans
                 , stepsLeft :: Int
                 , best :: (Trans, Double)
                 , curScore :: Double
                 }

startSearch :: Silhouette -> Solution -> Int -> IO Solution
startSearch sil sol maxSteps = do
  startScore <- score sil sol
  performSearch sil $ Step { trans = makeTrans sol, stepsLeft = maxSteps, best = (makeTrans sol, startScore), curScore = startScore }

performSearch :: Silhouette -> Step -> IO Solution
performSearch _ step@(Step { stepsLeft = 0 }) = stopSearch step
performSearch _ step@(Step { curScore = curScore }) | curScore >= 1 = stopSearch step
performSearch sil step@(Step { stepsLeft = stepsLeft, best = (bestTrans, bestScore), curScore = 0 }) =
    performSearch sil step { stepsLeft = stepsLeft - 1, trans = bestTrans, curScore = bestScore }
performSearch sil step = do
  -- putStrLn $ " ;; Performing search with current score = " ++ (show curScore) ++ ", " ++ (show stepsLeft) ++ " steps left"
  tryTrans <- randomAction 1.0 $ trans step
  tryScore <- score sil $ play tryTrans
  decideNext ((sqError tryScore) / (sqError $ curScore step)) tryTrans tryScore
      where
        sqError x = 1 / ((1 - x) * (1 - x))
        decideNext alpha tryTrans tryScore | alpha >= 1.0 = acceptNext tryTrans tryScore
        decideNext alpha tryTrans tryScore =
            do
              roll <- randomRIO (0.0, 1.0)
              -- putStrLn $ "  ;;; deciding: roll = " ++ (show roll) ++ ", alpha = " ++ (show alpha) ++ ", tryScore = " ++ (show tryScore)
              rollNext alpha roll tryTrans tryScore
        rollNext alpha roll | roll <= alpha = acceptNext
        rollNext _ _ = acceptCurr
        acceptNext tryTrans tryScore =
            performSearch sil $ step { trans = tryTrans, curScore = tryScore, stepsLeft = (stepsLeft step) - 1, best = updateBest }
        acceptCurr _ _ =
            performSearch sil $ step { stepsLeft = (stepsLeft step) - 1 }
        updateBest =
            case best step of
              (_, bestScore) | (curScore step) > bestScore -> (trans step, curScore step)
              other -> other

stopSearch :: Step -> IO Solution
stopSearch (Step { stepsLeft = stepsLeft, best = (bestTrans, bestScore) }) = do
  putStrLn $ " ;; Searching stops with best score = " ++ (show bestScore) ++ ", " ++ (show stepsLeft) ++ " steps left"
  return $ play bestTrans

solverMetropolis :: Problem -> IO Solution
solverMetropolis = solverMetropolisSteps 128

solverMetropolisSteps :: Int -> Problem -> IO Solution
solverMetropolisSteps maxSteps problem =
    startSearch (silhouette problem) (centrify problem initSolution) maxSteps
