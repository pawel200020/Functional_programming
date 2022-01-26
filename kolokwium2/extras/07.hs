subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x : xs) =  subsets xs ++ map (x :) (subsets xs)

removeDubs:: Eq a=>[a]->[a]
removeDubs []=[]
removeDubs (l:list) = l:(removeDubs (filter (/=l) list))