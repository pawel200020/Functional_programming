filter2:: [[Int]]-> [[Int]]
filter2  = filter (even . sum)

-- ce :: [[Int]] -> [Int]
-- ce = concatMap (\x -> if even $ sum x then x else [])

ce :: [[Int]] -> [Int]
--ce input = concat $ filter sumEven input
ce = concat.filter sumEven

sumEven :: [Int] -> Bool
--sumEven list = even (sum list)
sumEven = even.sum