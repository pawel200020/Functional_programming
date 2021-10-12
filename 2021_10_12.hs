maksimum :: [Int]-> Int 
maksimum [] = 0
maksimum (x:xs) = if x > maximum xs then x else maksimum xs

f :: [Int] -> (Int, Int)
f [x] = if x > 0 then (x, 0) else (0, x)
f (x : xs) = if x > 0 then (fst (f xs) + x, snd(f xs)) else (fst (f xs), snd(f xs) + x) 

commonPrefix :: String -> String -> String
commonPrefix a [] = a
commonPrefix [] b = b
commonPrefix (a:as) (b:bs) =  if (a == b) then [a] ++ commonPrefix as bs else []

-- int2Bin :: Int -> Int
-- int2Bin [0] = 0
-- int2Bin [1] = 1
-- Int2Bin x = (x `div` 2) ++ int2Bin [n `mod` 2]

merge :: [Int] -> [Int] -> [Int]
merge [] a = a
merge b [] = b
merge (a:as) (b:bs) = if a <= b then [a] ++ merge as (b:bs) else [b] ++ merge (a:as) bs

mergeSort :: [Int] -> [Int]
mergeSort [x] = [x]
mergeSort x = merge (mergeSort a) (mergeSort b)
    where
        a = take (div (length x) 2) x
        b = drop (div (length x) 2) x
