compref:: Eq a =>[a] -> [a] -> Int
-- compref list1 list2 = length (takeWhile (\(a,b)-> a==b) (zip list1 list2))
-- compref list1 list2 = length (takeWhile (uncurry (==)) (zip list1 list2))
-- compref list1 list2 = (length.takeWhile (uncurry (==))) (zip list1 list2)
-- compref list1 = ((length.takeWhile (uncurry (==))).zip list1)
compref = ((length.takeWhile (uncurry (==))).).zip 

