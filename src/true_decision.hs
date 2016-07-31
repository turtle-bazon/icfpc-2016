module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe (isNothing, isJust, fromJust)
import Data.List (intersect, reverse, take, zip, zipWith)
import qualified Data.ByteString as BS (ByteString, pack, unpack)
import qualified GHC.Word

-------------------------------------------------------------------------------

bp = B.pack
bu = B.unpack
nlines = splitAt
line ls = let (x,xs) = nlines 1 ls in (head x, xs)


route1 = [9, 11]
route2 = [4, 5, 6, 9, 11, 12]
route3 = [5, 6, 7, 8, 11, 12]

badPair:: Int -> Int -> Bool
badPair 1 x = elem x route1
badPair 2 x = elem x route2
badPair 3 x = elem x route3
badPair 4 x = elem x (fmap (\v -> mod (v + 3 - 1) 12 + 1) route1)
badPair 5 x = elem x (fmap (\v -> mod (v + 3 - 1) 12 + 1) route2)
badPair 6 x = elem x (fmap (\v -> mod (v + 3 - 1) 12 + 1) route3)
badPair 7 x = elem x (fmap (\v -> mod (v + 6 - 1) 12 + 1) route1)
badPair 8 x = elem x (fmap (\v -> mod (v + 6 - 1) 12 + 1) route2)
badPair 9 x = elem x (fmap (\v -> mod (v + 6 - 1) 12 + 1) route3)
badPair 10 x = elem x (fmap (\v -> mod (v + 9 - 1) 12 + 1) route1)
badPair 11 x = elem x (fmap (\v -> mod (v + 9 - 1) 12 + 1) route2)
badPair 12 x = elem x (fmap (\v -> mod (v + 9 - 1) 12 + 1) route3)

--genPermutataions :: [[Int]]
genPermutataions = permutataions [1..12]

--permutataions :: [Int] -> [[Int]]
permutataions [] = []
permutataions (x:[]) = [[x],[]]
permutataions (x:xs) =
	fmap (\p -> x:p) rest_permutations ++ rest_permutations
	where
		rest_permutations = permutataions xs

--permutationIsBad :: [Int] -> Bool
permutationIsBad [] = False
permutationIsBad (x:xs) =
	if hasBadPairInList x xs then True
	else permutationIsBad xs

--hasBadPairInList :: Int -> [Int] -> Bool
hasBadPairInList _ [] = False
hasBadPairInList x (y:ys) =
	if badPair x y then True
	else hasBadPairInList x ys


--contained :: [Int] -> [[Int]] -> Bool
contained xs ys =
	or $ fmap (\y -> xs /= y && xs == intersect xs y ) ys

--getGoodPermutations :: [[Int]]
getGoodPermutations =
	let
		good_permutations = filter (\p -> not $ permutationIsBad p ) genPermutataions
		big_permutations = filter (\p -> not $ contained p good_permutations) good_permutations
	in 
		big_permutations


permutationToVec :: [Int] -> [Int] -> [Int]
permutationToVec vs ps =
	map (\(n,v) -> if elem n ps then v else 0) $ zip [1..12] vs


solve' :: [Int] -> [Int] -> Int
solve' ns vs =
	max 0 $ solve ns (map (permutationToVec vs) getGoodPermutations) 1

solve :: [Int] -> [[Int]] -> Int -> Int
solve ns ps 11 = 
	maximum  $ ns

solve ns [] step = 
	maximum $ ns

solve ns (p:[]) step =
	maximum $ apply_lights ns p

solve ns (p:ps) step = 
	let
		ns' = apply_lights ns p
		res1 = solve ns' (p:ps) (step+1)
		res2 = solve ns ps step
	in
		if res1 < res2 then res1 else res2

apply_lights :: [Int] -> [Int] -> [Int]
apply_lights ns lights =
	zipWith (-) ns lights
		
apply_permutation ns vs ps = 
	apply_perm (zip ns [1..12]) vs ps

apply_perm [] vs ps = []
apply_perm ((n,pos):ns) vs ps =
	if elem pos ps then (max 0 (n - vs!!(pos-1))) : rest_result
	else n : rest_result
	where
		rest_result = apply_perm ns vs ps

run :: [B.ByteString] -> String
run in0 =
	let
		(l1, in1) = line in0
		(l2, in2) = line in1
		ns = fmap (fst . fromJust . B.readInt) $  B.words $ l1
		vs = fmap (fst . fromJust . B.readInt) $  B.words $ l2
	in
		show $ solve' ns vs
		--show $ solve' ns vs getGoodPermutations 1

main :: IO ()
main = getContents >>= putStrLn . run . B.lines . bp
