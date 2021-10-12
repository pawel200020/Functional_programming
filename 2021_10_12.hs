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