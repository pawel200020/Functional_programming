-- func1 x l = map (\y -> y * x) l
-- func1 x = map ((*) x)
func1 = map.(*)

-- func2 f g l = filter f (map g l)
-- func2 f g l = ((filter f).(map g)) l
-- func2 f g = ((filter f).(map g))
-- func2 f g = (filter f).map g
-- func2 f g = (((filter f).).(map)) g
-- func2 f = (filter f.).map
func2 :: (a1 -> Bool) -> (a2 -> a1) -> [a2] -> [a1]
func2 f = ((.map).)(.)(filter f)