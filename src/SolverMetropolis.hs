module SolverMetropolis (solverMetropolis) where

import qualified Data.Number.FixedFunctions as FF
import System.Random
import Common
import Math
import Figures
import SolverBBSimple
import Parse
import Show

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

randomTranslationPoint :: Double -> IO Point
randomTranslationPoint variation = do
  deltaX <- randomRIO (-variation, variation)
  deltaY <- randomRIO (-variation, variation)
  return $ Point { px = approx deltaX, py = approx deltaY }

randomRotationAngle :: Double -> IO Number
randomRotationAngle variation = do
  delta <- randomRIO (-variation, variation)
  return $ approx $ pi * 2 * delta

randomFoldPart :: Double -> IO Number
randomFoldPart variation = do
  part <- randomRIO (0, variation)
  return $ approx $ part / 2

partSegment :: Number -> Number -> Number -> Number
partSegment part start end =
    start + part * (end - start)

foldPartLeft :: Number -> Solution -> Solution
foldPartLeft part solution =
    foldLeft foldPos solution
        where
          (Point { px = start }, Point { px = end }) = bbox $ dstPoly solution
          foldPos = partSegment (1 - part) start end

foldPartRight :: Number -> Solution -> Solution
foldPartRight part solution =
    foldRight foldPos solution
        where
          (Point { px = start }, Point { px = end }) = bbox $ dstPoly solution
          foldPos = partSegment part start end

foldPartDown :: Number -> Solution -> Solution
foldPartDown part solution =
    foldDown foldPos solution
        where
          (Point { py = start }, Point { py = end }) = bbox $ dstPoly solution
          foldPos = partSegment (1 - part) start end

foldPartUp :: Number -> Solution -> Solution
foldPartUp part solution =
    foldUp foldPos solution
        where
          (Point { py = start }, Point { py = end }) = bbox $ dstPoly solution
          foldPos = partSegment part start end

data Trans = Trans { baseSolution :: Solution
                   , foldHistory :: [Solution -> Solution]
                   , transHistory :: [Solution -> Solution]
                   }

makeTrans :: Solution -> Trans
makeTrans sol =
    Trans { baseSolution = sol, foldHistory = [], transHistory = [] }

play :: Trans -> (Solution, Int)
play trans = (sol, length $ showSolution $ sol)
    where
      sol = playSpecific (baseSolution trans) $ (transHistory trans) ++ (foldHistory trans)

playSpecific :: Solution -> [Solution -> Solution] -> Solution
playSpecific = foldr run
    where run f solution = f solution

randomAction :: Double -> Trans -> IO Trans
randomAction variation tr =
  randomRIO (0, 5) >>= choose
      where
        choose :: Int -> IO Trans
        choose 0 = do
          point <- randomTranslationPoint variation
          -- putStrLn $ "   ;;; action: translate on " ++ (show point)
          return $ tr { transHistory = (translate point) : (transHistory tr) }
        choose 1 = do
          angle <- randomRotationAngle variation
          -- putStrLn $ "   ;;; action: rotate by " ++ (show angle)
          return $ tr { transHistory = (rotateAroundCenter angle) : (transHistory tr) }
        choose 2 = do
          part <- randomFoldPart variation
          -- putStrLn $ "   ;;; action: fold left by " ++ (show part)
          return $ tr { foldHistory = (foldPartLeft part) : (foldHistory tr) }
        choose 3 = do
          part <- randomFoldPart variation
          -- putStrLn $ "   ;;; action: fold right by " ++ (show part)
          return $ tr { foldHistory = (foldPartRight part) : (foldHistory tr) }
        choose 4 = do
          part <- randomFoldPart variation
          -- putStrLn $ "   ;;; action: fold down by " ++ (show part)
          return $ tr { foldHistory = (foldPartDown part) : (foldHistory tr) }
        choose 5 = do
          part <- randomFoldPart variation
          -- putStrLn $ "   ;;; action: fold up by " ++ (show part)
          return $ tr { foldHistory = (foldPartUp part) : (foldHistory tr) }
        choose _ = error "shoud not get here"
        rotateAroundCenter angle solution =
            rotate (centrifySolution solution) angle solution

data Step = Step { trans :: Trans
                 , stepsLeft :: Int
                 , best :: (Trans, Double)
                 , curScore :: Double
                 , curLength :: Int
                 }

startSearch :: Silhouette -> Solution -> Int -> IO Solution
startSearch sil sol maxSteps = do
  startScore <- score sil sol
  performSearch sil $ Step { trans = makeTrans sol
                           , stepsLeft = maxSteps
                           , best = (makeTrans sol, startScore)
                           , curScore = startScore
                           , curLength = length $ showSolution sol
                           }

performSearch :: Silhouette -> Step -> IO Solution
performSearch _ step@(Step { stepsLeft = 0 }) = stopSearch step
performSearch _ step@(Step { curScore = curScore }) | curScore >= 1 = stopSearch step
performSearch sil step@(Step { stepsLeft = stepsLeft, best = (bestTrans, bestScore), curScore = 0 }) =
    performSearch sil step { stepsLeft = stepsLeft - 1, trans = bestTrans, curScore = bestScore }
performSearch sil step@(Step { trans = Trans { baseSolution = sol }, curLength = curLength }) | curLength > 5000 = do
  resetScore <- score sil sol
  performSearch sil step { trans = makeTrans sol, curScore = resetScore, curLength = length $ showSolution sol }
performSearch sil step = do
  -- putStrLn $ " ;; score = " ++ (show $ curScore step) ++ ", " ++ (show $ stepsLeft step) ++ " left, sol length = " ++ (show $ curLength step)
  tryTrans <- randomAction 1.0 $ trans step
  let (trySol, tryLen) = play tryTrans
  tryScore <- score sil trySol
  decideNext ((sqError tryScore) / (sqError $ curScore step)) tryTrans tryScore tryLen
      where
        sqError x = 1 / ((1 - x) * (1 - x))
        decideNext alpha tryTrans tryScore tryLen | alpha >= 1.0 = acceptNext tryTrans tryScore tryLen
        decideNext alpha tryTrans tryScore tryLen =
            do
              roll <- randomRIO (0.0, 1.0)
              -- putStrLn $ "  ;;; deciding: roll = " ++ (show roll) ++ ", alpha = " ++ (show alpha) ++ ", tryScore = " ++ (show tryScore)
              rollNext alpha roll tryTrans tryScore tryLen
        rollNext alpha roll | roll <= alpha = acceptNext
        rollNext _ _ = acceptCurr
        acceptNext tryTrans tryScore tryLen =
            performSearch sil $ step { trans = tryTrans, curScore = tryScore, stepsLeft = (stepsLeft step) - 1, best = updateBest, curLength = tryLen }
        acceptCurr _ _ _ =
            performSearch sil $ step { stepsLeft = (stepsLeft step) - 1 }
        updateBest =
            case best step of
              (_, bestScore) | (curScore step) > bestScore -> (trans step, curScore step)
              other -> other

stopSearch :: Step -> IO Solution
stopSearch (Step { stepsLeft = stepsLeft, best = (bestTrans, bestScore), curLength = len }) = do
  -- putStrLn $ " ;; Done with best score = " ++ (show bestScore) ++ ", " ++ (show stepsLeft) ++ " steps left, solution length = " ++ (show len)
  -- putStrLn $ " ;; Fold history length = " ++ (show $ length $ foldHistory bestTrans)
  -- putStrLn $ " ;; Trans history length = " ++ (show $ length $ transHistory bestTrans)
  return $ fst $ play bestTrans

solverMetropolis :: Problem -> IO Solution
solverMetropolis = solverMetropolisSteps 128

solverMetropolisSteps :: Int -> Problem -> IO Solution
solverMetropolisSteps maxSteps problem =
    startSearch (silhouette problem) (centrify problem initSolution) maxSteps
