module SolverMinimalRect (solverMinimalRect) where

import qualified Data.Number.FixedFunctions as FF
import Common
import Math
import SolverBBSimple
import SolverBBRect
import Figures
import Debug.Trace
import System.IO.Unsafe

foldPlaces target_len =
    folds' target_len 1

folds' target_len current_len =
    if
        target_len < (current_len/2)
    then
        (current_len/2) : (folds' target_len (current_len/2))
    else
        [current_len - target_len]

foldRect :: Number -> Number -> Solution
foldRect width height =
    let
        folded_h = if width < 1
            then
                foldl (\solution -> \place -> foldLeft place solution) initSolution $ foldPlaces width
            else
                initSolution

        folded_v = if height < 1
            then
                foldl (\solution -> \place -> foldDown place solution) folded_h $ foldPlaces height
            else
                folded_h
    in
        folded_v

foldMininalRectangleSolution :: Problem -> Number -> Solution
foldMininalRectangleSolution problem alpha =
    let
        polygons = map polygon $ filter isFillPoly $ silhouette problem
        (fsin, fcos) = calcTrig alpha
        pnts = map (rotatePoint' (Point 0 0) fsin fcos) $ foldl (\acc -> \ps -> ps ++ acc) [] polygons
        minX = minimum $ map px pnts
        minY = minimum $ map py pnts
        maxX = maximum $ map px pnts
        maxY = maximum $ map py pnts
        width = maxX - minX
        height = maxY - minY
        solution = foldRect width height
        minSolX = minimum $ map (px.dstvertex) $ points solution
        minSolY = minimum $ map (py.dstvertex) $ points solution
    in
        rotate (Point 0 0) (-alpha) $ translate (Point (-minSolX+minX) (-minSolY+minY)) solution
    where
        isFillPoly (PolyFill _) = True
        isFillPoly (PolyHole _) = False



score' :: Problem -> Solution -> Double
score' problem solution = unsafePerformIO $ score (silhouette problem) solution

makeMininalRectangleSolution :: Problem -> Solution
makeMininalRectangleSolution problem =
    let
        alpha_steps = 200
        alphas = [i * (toRational pi) / alpha_steps | i <- [0..alpha_steps]]
        --alphas = [0]
        solutions = map (foldMininalRectangleSolution problem) alphas
        sscore = score' problem
        (best_score, best_solution) = foldl (\(best_score, best_sol) -> \sol -> let new_score=sscore sol in if new_score > best_score then (new_score,sol) else (best_score, best_sol) ) (sscore (head solutions), head solutions) solutions
    in
        best_solution


solverMinimalRect :: Problem -> IO Solution
solverMinimalRect = return . makeMininalRectangleSolution
