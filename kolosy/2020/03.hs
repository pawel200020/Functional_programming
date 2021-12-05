
-- doNPozycji:: Int-> [a] -> [a]
-- doNPozycji 0 (x:xs) = [x]
-- doNPozycji n (x:xs) = x:doNPozycji (n-1) xs

-- listOfLoverVals:: Int -> [Int]
-- listOfLoverVals  0 = []
-- listOfLoverVals x = takeWhile (<x) [1..]

-- hasValue:: [Int]-> Int-> Bool
-- hasValue [] _ = False 
-- hasValue (x:xs) v = if(x==v) then True else hasValue xs v

-- hasAllLoverVals:: [Int]->Int-> [Bool]
-- hasAllLoverVals [] _ = [True]
-- hasAllLoverVals list n = if(length list /= n-1) then [False] else map(\x -> hasValue list x) [1..n-1]

-- hasAllLoverVals2 :: [Bool]-> Bool 
-- hasAllLoverVals2 list =  length (filter (==True) list) == length list


-- cp :: [Int] -> [Int]
-- cp [] = []
-- cp list = filter (\x->hasAllLoverVals2(hasAllLoverVals take  x)) list
 