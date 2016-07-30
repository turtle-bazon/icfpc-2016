module Training where

route1 = [9, 11]
route2 = [4, 5, 6, 9, 11, 12]
route3 = [5, 6, 7, 8, 11, 12]

badPair:: Int -> Int -> Bool
badPair 1 x = elem x route1
badPair 2 x = elem x route2
badPair 3 x = elem x route3
badPair 4 x = elem x (map (\v -> mod (v + 3 - 1) 12 + 1) route1)
badPair 5 x = elem x (map (\v -> mod (v + 3 - 1) 12 + 1) route2)
badPair 6 x = elem x (map (\v -> mod (v + 3 - 1) 12 + 1) route3)
badPair 7 x = elem x (map (\v -> mod (v + 6 - 1) 12 + 1) route1)
badPair 8 x = elem x (map (\v -> mod (v + 6 - 1) 12 + 1) route2)
badPair 9 x = elem x (map (\v -> mod (v + 6 - 1) 12 + 1) route3)
badPair 10 x = elem x (map (\v -> mod (v + 9 - 1) 12 + 1) route1)
badPair 11 x = elem x (map (\v -> mod (v + 9 - 1) 12 + 1) route2)
badPair 12 x = elem x (map (\v -> mod (v + 9 - 1) 12 + 1) route3)

genPermutataions :: [[Int]]
genPermutataions = permutataions [1..12]

permutataions :: [Int] -> [[Int]]
permutataions [] = []
permutataions (x:[]) = [[x],[]]
permutataions (x:xs) =
	map (\p -> x:p) rest_permutations ++ rest_permutations
	where
		rest_permutations = permutataions xs

permutationIsBad :: [Int] -> Bool
permutationIsBad [] = False
permutationIsBad (x:xs) =
	if hasBadPairInList x xs then True
	else permutationIsBad xs

hasBadPairInList :: Int -> [Int] -> Bool
hasBadPairInList _ [] = False
hasBadPairInList x (y:ys) =
	if badPair x y then True
	else hasBadPairInList x ys

getGoodPermutations :: [[Int]]
getGoodPermutations =
	filter (\p -> not $ permutationIsBad p ) genPermutataions
