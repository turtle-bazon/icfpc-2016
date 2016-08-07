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

data HistoryLine = History { hindex :: Int, action :: Solution -> Solution }

data Trans = Trans { baseSolution :: Solution
                   , foldHistory :: [HistoryLine]
                   , transHistory :: [HistoryLine]
                   }

makeTrans :: Solution -> Trans
makeTrans sol =
    Trans { baseSolution = sol, foldHistory = [], transHistory = [] }

nextIndex :: Trans -> Int
nextIndex (Trans { foldHistory = [], transHistory = [] }) = 0
nextIndex (Trans { foldHistory = History { hindex = i } : _, transHistory = [] }) = i + 1
nextIndex (Trans { foldHistory = [], transHistory = History { hindex = i } : _ }) = i + 1
nextIndex (Trans { foldHistory = History { hindex = ia } : _, transHistory = History { hindex = ib } : _ }) = (max ia ib) + 1

dropRecent :: Trans -> Trans
dropRecent tr@(Trans { foldHistory = [], transHistory = [] }) = tr
dropRecent tr@(Trans { foldHistory = _ : rest, transHistory = [] }) = tr { foldHistory = rest }
dropRecent tr@(Trans { foldHistory = [], transHistory = _ : rest }) = tr { transHistory = rest }
dropRecent tr@(Trans { foldHistory = History { hindex = ia } : _, transHistory = History { hindex = ib } : rest })
    | ia < ib = tr { transHistory = rest }
dropRecent tr@(Trans { foldHistory = _ : rest }) = tr { foldHistory = rest }

dropRecentTimes :: Int -> Trans -> Trans
dropRecentTimes 0 tr = tr
dropRecentTimes count tr = dropRecentTimes (count - 1) (dropRecent tr)

play :: Trans -> (Solution, Int)
play trans = (sol, length $ showSolution $ sol)
    where
      sol = playSpecific (baseSolution trans) $ map action $ (transHistory trans) ++ (foldHistory trans)

playSpecific :: Solution -> [Solution -> Solution] -> Solution
playSpecific = foldr run
    where run f solution = f solution

rememberFold :: (Solution -> Solution) -> Trans -> Trans
rememberFold action tr =
    tr { foldHistory = History { hindex = nextIndex tr, action = action } : (foldHistory tr) }

rememberTrans :: (Solution -> Solution) -> Trans -> Trans
rememberTrans action tr =
    tr { transHistory = History { hindex = nextIndex tr, action = action } : (transHistory tr) }

randomAction :: Double -> Trans -> IO Trans
randomAction variation tr =
  randomRIO (0, 5) >>= choose
      where
        choose :: Int -> IO Trans
        choose 0 = do
          point <- randomTranslationPoint variation
          -- putStrLn $ "   ;;; action: translate on " ++ (show point)
          return $ rememberTrans (translate point) tr
        choose 1 = do
          angle <- randomRotationAngle variation
          -- putStrLn $ "   ;;; action: rotate by " ++ (show angle)
          return $ rememberTrans (rotateAroundCenter angle) tr
        choose 2 = do
          part <- randomFoldPart variation
          -- putStrLn $ "   ;;; action: fold left by " ++ (show part)
          return $ rememberFold (foldPartLeft part) tr
        choose 3 = do
          part <- randomFoldPart variation
          -- putStrLn $ "   ;;; action: fold right by " ++ (show part)
          return $ rememberFold (foldPartRight part) tr
        choose 4 = do
          part <- randomFoldPart variation
          -- putStrLn $ "   ;;; action: fold down by " ++ (show part)
          return $ rememberFold (foldPartDown part) tr
        choose 5 = do
          part <- randomFoldPart variation
          -- putStrLn $ "   ;;; action: fold up by " ++ (show part)
          return $ rememberFold (foldPartUp part) tr
        choose _ = error "shoud not get here"
        rotateAroundCenter angle solution =
            rotate (centrifySolution solution) angle solution

data Best = Best { bestTrans :: Trans
                 , bestScore :: Double
                 , bestLength :: Int
                 , fallback :: Int
                 }

fallbackBest :: Silhouette -> Best -> IO (Best, Best)
fallbackBest sil best@(Best { fallback = 0 }) = return $ (best { fallback = 1 }, best)
fallbackBest sil best =
    let
        rewoundTrans = dropRecentTimes (fallback best) $ bestTrans best
        (rewoundSol, rewoundLen) = play rewoundTrans
    in
      do
        newScore <- score sil rewoundSol
        -- putStrLn $ "  ;;; falling back best @ " ++ (show $ fallback best) ++ ": " ++ (show $ bestScore best) ++ " -> " ++ (show newScore)
        return $ ( best { fallback = (fallback best) + 1 }
                 , Best { bestTrans = rewoundTrans
                        , bestScore = newScore
                        , bestLength = rewoundLen
                        , fallback = 0
                        }
                 )

data Step = Step { trans :: Trans
                 , stepsLeft :: Int
                 , best :: Best
                 , curScore :: Double
                 , curLength :: Int
                 }

startSearch :: Silhouette -> Solution -> Int -> IO Solution
startSearch sil sol maxSteps = do
  startScore <- score sil sol
  performSearch sil $ Step { trans = makeTrans sol
                           , stepsLeft = maxSteps
                           , best = Best { bestTrans = makeTrans sol
                                         , bestScore = startScore
                                         , bestLength = solLen
                                         , fallback = 0
                                         }
                           , curScore = startScore
                           , curLength = solLen
                           }
      where
        solLen = length $ showSolution sol

fallbackSearch :: Silhouette -> Step -> IO Solution
fallbackSearch sil step@(Step { stepsLeft = stepsLeft, best = best }) = do
    (curBest, Best { bestTrans = curTrans, bestScore = curScore, bestLength = curLength }) <- fallbackBest sil best
    performSearch sil step { stepsLeft = stepsLeft - 1, trans = curTrans, curScore = curScore, curLength = curLength, best = curBest }

describeStep :: Step -> IO ()
describeStep step =
    putStrLn (  " ;; score = " ++ (show $ curScore step)
             ++ " (best = " ++ (show $ bestScore $ best step) ++ "), "
             ++ (show $ stepsLeft step) ++ " left"
             ++ ", sol length = " ++ (show $ curLength step)
             ++ ", best rewind cnt = " ++ (show $ fallback $ best $ step)
             )

performSearch :: Silhouette -> Step -> IO Solution
performSearch _ step@(Step { stepsLeft = 0 }) = stopSearch step
performSearch _ step@(Step { curScore = curScore }) | curScore >= 1 = stopSearch step
performSearch sil step@(Step { curScore = 0 }) = fallbackSearch sil step
performSearch sil step@(Step { stepsLeft = stepsLeft, best = best, curLength = curLength }) | curLength > 5000 = fallbackSearch sil step
performSearch sil step = do
  -- describeStep step
  let variance = 1.0 - (curScore step)
  tryTrans <- randomAction variance $ trans step
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
        acceptNext tryTrans tryScore tryLen = do
            performSearch sil $ step { trans = tryTrans
                                     , curScore = tryScore
                                     , stepsLeft = (stepsLeft step) - 1
                                     , best = updateBest Best { bestTrans = tryTrans
                                                              , bestScore = tryScore
                                                              , bestLength = tryLen
                                                              , fallback = 0
                                                              }
                                     , curLength = tryLen
                                     }
        acceptCurr _ _ _ =
            performSearch sil $ step { stepsLeft = (stepsLeft step) - 1 }
        updateBest newBest@Best { bestScore = tryScore, bestLength = tryLength }
            | tryScore > (bestScore $ best step) && tryLength <= 5000 = newBest
        updateBest _ = best step

stopSearch :: Step -> IO Solution
stopSearch (Step { stepsLeft = stepsLeft, best = best }) = do
  -- putStrLn $ " ;; Done with best score = " ++ (show $ bestScore best) ++ ", " ++ (show stepsLeft) ++ " steps left, solution length = " ++ (show $ bestLength best)
  -- putStrLn $ " ;; Fold history length = " ++ (show $ length $ foldHistory bestTrans)
  -- putStrLn $ " ;; Trans history length = " ++ (show $ length $ transHistory bestTrans)
  return $ fst $ play $ bestTrans best

solverMetropolis :: Problem -> IO Solution
solverMetropolis = solverMetropolisSteps 128

solverMetropolisSteps :: Int -> Problem -> IO Solution
solverMetropolisSteps maxSteps problem =
    startSearch (silhouette problem) (centrify problem initSolution) maxSteps
