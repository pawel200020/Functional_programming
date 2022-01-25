f ::[Int]-> Int
f list = foldl(\acc x-> acc+x) 0 list


odwr3 :: [a] -> [a]
odwr3 list = foldl (\acc x -> x:acc) [] list