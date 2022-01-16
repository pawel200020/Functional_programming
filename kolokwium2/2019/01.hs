filter2:: [[Int]]-> [[Int]]
filter2  = filter (even . sum)

ce :: [[Int]] -> [Int]
ce = concatMap (\x -> if even $ sum x then x else [])