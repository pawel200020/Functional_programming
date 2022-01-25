-- f:: [Double]->Double
-- f[] = 1
-- f list = (((+).(/) 1).(f.tail)) list $ head list

f :: [Double] -> Double
f numbers = foldr(\number acc -> number + 1/acc) 1 numbers
--f = foldr(\number acc -> number + 1/acc) 1 
--f = foldr(\number acc -> number + (/) 1 acc) 1 
--f = foldr(\number -> number + (/) 1) 1 
------
--f = foldr(\number acc -> (+) number (1/acc)) 1 
--f = foldr(\number acc -> (+) number ((/) 1 acc)) 1 
--f = foldr(\number acc -> (+) ((/) 1 acc) number) 1 
-- f = foldr(\number acc -> flip g number acc) 1 
--     where g u v = (+) ((/) 1 u) v
-- f = foldr(flip g) 1 
--     where g u = (+) ((/) 1 u)
-- f = foldr(flip g) 1 
--     where g = (+).(/) 1
--f = foldr(flip ((+).(/) 1)) 1 