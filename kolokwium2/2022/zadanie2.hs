f :: [(Int,Int)]-> Int -> Int
--f numbers u = foldr(\number acc -> (fst number - snd number) * acc) u numbers
--f numbers u = foldr(\number ->(*) (uncurry (-) number) ) u numbers
-- f numbers u = foldr((*) . uncurry (-) ) u numbers
-- f numbers u = foldr((*) . uncurry (-) ) u numbers
-- f numbers u = flip g numbers u
--                 where g u numbers= foldr((*) . uncurry (-) ) u 
-- f numbers u = flip g numbers u
--                 where g  = foldr((*) . uncurry (-) 
f  = flip g 
                where g  = foldr((*) . uncurry (-) )
