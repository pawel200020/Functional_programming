divBy3:: Int-> Bool
divBy3 num = num `mod` 3 ==0

divBy32:: Int-> Bool
divBy32 = (0 ==) . (`mod` 3)

countDiv3:: [Int]->Int
countDiv3 = length.filter(divBy32) 

f2 :: [[Int]] -> [[Int]]
f2  = filter(divBy32 . countDiv3)

f :: [[Int]] -> Int
f = length.f2 