module Show where

import Data.Ratio
import Data.List
import Data.Maybe
import Common

showNumber :: Number -> String
showNumber v =
    case denominator v of
      1 -> show $ numerator v
      d -> (show $ numerator v) ++ "/" ++ (show d)

showPoint :: Point -> String
showPoint p =
    (showNumber $ px p) ++ "," ++ (showNumber $ py p)

showIndexedPoint :: IndexedPoint -> String
showIndexedPoint ip =
  "[" ++ (showPoint $ srcvertex ip) ++ ", " ++ (showPoint $ dstvertex ip) ++ "]"

showEdge :: Edge -> String
showEdge (a, b) =
    (showPoint a) ++ " " ++ (showPoint b)

showSolution :: Solution -> String
showSolution sol =
    showPoints ++ showFacets
        where
          showPoints = (showTotal $ points sol) ++ (showList $ map showIndexedPoint $ points sol)
          showFacets = (showTotal $ facets sol) ++ (showList $ map showFacet $ facets sol)
          showFacet indices = unwords $ (show $ length indices) : (map show indices)
          showList = foldr (\v acc -> v ++ "\n" ++ acc) ""
          showTotal l = (show $ length l) ++ "\n"
