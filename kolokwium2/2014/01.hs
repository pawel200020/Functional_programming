f:: [Int]->[[Int]]
f (a:array) = foldl(\acc x -> if (last.last) acc + 1 == x then init acc++[last acc++[x]] else acc++[[x]]) [[a]] array

--2013
getNth:: Int->[Int]->Int 
getNth 1 list = head list
getNth number list = getNth ( number - 1) (tail list)

getNth1:: Int->[Int]->Int 
getNth1 0 list = head list
getNth1 number list = (getNth ((-) number 1)) (tail list)

getNth2:: Int->[Int]->Int 
getNth2 1  = head 
getNth2 number  = getNth(flip (-) 1 number).tail

-- select::Int->[[Int]]->[Int]
-- selest int array = map (\x-> gethNth2) 