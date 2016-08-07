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

startSearch :: Silhouette -> Solution -> IO Solution
startSearch sil sol =
    score sil sol >>= performSearch sil sol

performSearch :: Silhouette -> Solution -> Double -> IO Solution
performSearch sil sol curScore | curScore >= 1.0 = return sol
performSearch sil sol curScore =
    undefined


solverRandomSearch :: Problem -> IO Solution
solverRandomSearch problem =
    startSearch (silhouette problem) (centrify problem initSolution)
