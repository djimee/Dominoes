numOccurrences :: Ord a => a -> [a] -> Integer
numOccurrences _ [] = 0
numOccurrences n list = sum $ map (\a -> 1) $ filter (== n) list

countss :: Integer -> [Integer] -> [Integer]
countss n list = [x | x <- [0..6], numOccurrences x list == n]
-- gets spot values that have 0 occurrences

weakDominoesL :: [Domino] -> [Domino] -> [Domino]
weakDominoesL domSet doms@(o:os) = [(l,r) | (l,r) <- doms, l == o] : weakDominoes domSet os
-- ++ [(l,r) | (l,r) <- doms, r `elem` occurrences]

filter (\x -> )