h:: [Int] -> [Int] 
--h list = map snd (filter (\(x,y)-> (x `mod` 2) == 1) (zip [1..] list))
-- h list = map snd (filter (\pair-> ((==1).(flip mod 2)) (fst pair) ) (zip [1..] list))
-- h list = map snd (filter (\pair-> (((==1).(flip mod 2)).fst) pair ) (zip [1..] list))
-- h list = ((map snd).((filter (((==1).(flip mod 2)).fst)).zip [1..])) list
h = (map snd).((filter (((==1).(flip mod 2)).fst)).zip [1..])
