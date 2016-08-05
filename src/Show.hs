module Show where

import Data.Ratio
import Common

showNumber :: Number -> String
showNumber v =
    case denominator v of
      1 -> show $ numerator v
      d -> (show $ numerator v) ++ "/" ++ (show d)

showPoint :: Point -> String
showPoint p =
    (showNumber $ px p) ++ "," ++ (showNumber $ py p)

showEdge :: Edge -> String
showEdge (a, b) =
    (showPoint a) ++ " " ++ (showPoint b)

showSolution :: Solution -> String
showSolution sol =
    showSrc ++ showFacets ++ showDst
        where
          showSrc = (showTotal $ src sol) ++ (showLst $ map (showPoint . vertex) $ src sol)
          showFacets = (showTotal $ facets sol) ++ (showLst $ map showFacet $ facets sol)
          showFacet indices = unwords $ (show $ length indices) : (map show indices)
          showDst = showLst $ map showPoint $ dst sol
          showLst = foldr (\v acc -> v ++ "\n" ++ acc) ""
          showTotal l = (show $ length l) ++ "\n"
