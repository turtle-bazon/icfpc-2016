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

dstPoly :: Solution -> Poly
dstPoly = map dstvertex . points

centrify :: Problem -> Solution -> Solution
centrify problem solution =
    translate relativeCenter solution
        where
          relativeCenter = translatePoint solutionDelta problemCenter
          problemCenter = centrifyPos problem
          solutionDelta = negatePoint $ centrifyPoly $ dstPoly solution

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
