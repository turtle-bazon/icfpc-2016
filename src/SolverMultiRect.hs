module SolverMultiRect (solverMultiRect) where

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

moveSolution :: Number -> Number -> Number -> Solution -> Solution
moveSolution alpha minX minY solution =
    let
        minSolX = minimum $ map (px.dstvertex) $ points solution
        minSolY = minimum $ map (py.dstvertex) $ points solution
    in
        rotate (Point 0 0) (-alpha) $ translate (Point (-minSolX+minX) (-minSolY+minY)) solution

foldMininalRectangleSolution :: Problem -> Number -> Number -> [Solution]
foldMininalRectangleSolution problem alpha step =
    let
        polygons = map polygon $ filter isFillPoly $ silhouette problem
        pnts = map (rotatePoint (Point 0 0) alpha) $ foldl (\acc -> \ps -> ps ++ acc) [] polygons
        minX = minimum $ map px pnts
        minY = minimum $ map py pnts
        maxX = maximum $ map px pnts
        maxY = maximum $ map py pnts
        width = maxX - minX
        height = maxY - minY
        solution = foldRect width height
        solutionsA = map (\i -> foldRect (i*width/step) height) [1..(step-1)]
        solutionsB = map (\i -> foldRect width (i*height/step)) [1..(step-1)]
        
        solution' = moveSolution alpha minX minY solution 
        solutionsA' = map (moveSolution alpha minX minY) solutionsA
        solutionsB' = map (moveSolution alpha minX minY) solutionsB
    in
        [solution'] ++ solutionsA' ++ solutionsB'
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
        solutions = foldl (\acc -> \alpha -> (alpha, (head $ foldMininalRectangleSolution problem alpha 0)) : acc ) [] alphas
        sscore = score' problem
        (best_score, (best_a, best_solution)) = foldl (\(best_score, (best_alpha, best_sol)) -> \(alpha, sol) -> let new_score=sscore sol in if new_score > best_score then (new_score, (alpha, sol)) else (best_score, (best_alpha,best_sol)) ) (sscore (snd $ head solutions), head solutions) solutions
        
        new_sols = (best_a, best_solution) : ( map (\s -> (best_a,s)) $ foldMininalRectangleSolution problem best_a 25 )
        (best_score', (best_a', best_solution')) = foldl (\(best_score, (best_alpha, best_sol)) -> \(alpha, sol) -> let new_score=sscore sol in if new_score > best_score then (new_score, (alpha, sol)) else (best_score, (best_alpha,best_sol)) ) (sscore (snd $ head new_sols), head new_sols) new_sols
    in
        best_solution'


solverMultiRect :: Problem -> IO Solution
solverMultiRect = return . makeMininalRectangleSolution
