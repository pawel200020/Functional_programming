divby3:: Int -> Bool
--divby3 num = num `mod` 3 == 0
--divby3 num = (==0) (num `mod` 3)
-- divby3 num = (==0) ((flip mod) 3 num)
-- divby3 num = ((==0).((flip mod) 3)) num
divby3 = (==0).flip mod 3

countdivby3:: [Int]-> Int 
countdivby3 = length.filter divby3

f::[[Int]]-> Int 
--f list = length (filter(\x-> divby3(countdivby3 x)) list)
-- f list = length (filter(\x-> (divby3.countdivby3) x) list)
-- f list = length (filter(divby3.countdivby3) list)
-- f list = (length.filter(divby3.countdivby3)) list
f = length.filter(divby3.countdivby3)