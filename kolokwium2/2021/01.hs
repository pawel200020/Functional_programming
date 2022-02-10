-- f list = zipWith (curry (\(a,b) -> 3 * (a+b))) list [1..10]

-- f2 list = map (\(a,b) -> 3 * (a+b)) (zip list [1..10])

-- f3 list = map (\(a,b) -> 3 * (+) a b) (zip list [1..10])

-- f4 list = map (\(a,b) -> 3 * (+) a b) (zip list [1..10])

-- f5 list = map (\(a,b) -> 3 * uncurry (+) (a,b)) (zip list [1..10])

-- f6 list = flip (zipWith (curry ((*3).uncurry (+)))) [1..10]

f :: [Int] -> [Int]
-- f list = map (\(a,b) -> 3*(a+b)) (zip list [1..10])
--f list = map (\(a,b) -> 3*(a+b)) (zip list [1..10])
--f list = map (\(a,b) -> 3*(a+b)) (flip zip [1..10] list)
--f list= map (\(a,b) -> 3*(a+b)) (flip zip [1..10] list)
--f list= map (\number -> 3*(uncurry (+) number)) (flip zip [1..10] list)
--f list= map (\number -> 3*uncurry (+) number) (flip zip [1..10] list)
--f list= map ((3*).uncurry (+)) (flip zip [1..10] list)
--f list= (map ((3*).uncurry (+))) (flip zip [1..10] list)
--f list= (map ((3*).uncurry (+)).flip zip [1..10]) list
f = ((map ((*3).(uncurry (+)))).flip zip [1..10]) 