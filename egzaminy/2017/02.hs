isLetter:: Char->Bool
isLetter c = if(length (filter (\x->x==c) (take 26 ['a','b'..]))>0) || (length (filter (\x->x==c) (take 26 ['A','B'..]))>0) then True else False

listOfStrign:: String -> [String]
listOfStrign (a:array) = foldl(\acc x-> if isLetter x then (take ((length acc)-1) acc) ++[last acc++[x]] else if x==' ' then acc++[[]]else acc) [[a]] array

hasElement:: [String]->String->Bool 
hasElement list elem = length(filter (\x->x==elem) list)>0

distinctElement:: String->[String]
distinctElement array = foldl(\acc x -> if(hasElement acc x) then acc else acc++[x]) [] (listOfStrign array)

howElement:: [String]->String->Int 
howElement list elem = length(filter (\x->x==elem) list)

countElem:: String -> [(Int, String)]
countElem word = tail (foldl(\acc x -> acc++[((howElement (listOfStrign word) x),x)]) [(0,"a")] (distinctElement word))

qsort :: [(Int, String)]->[(Int, String)]
qsort [] = []
qsort (x:xs) = left ++ [x] ++ right
    where 
        left = qsort  [a | a <- xs, fst a > fst x]
        right = qsort [a | a <- xs, fst a <= fst x]

elemSum:: String ->[(Int, String)]
elemSum word = qsort (countElem word)