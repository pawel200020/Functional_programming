quantityArray::Eq a => [a] -> [(a, Int)]
quantityArray []=[]
quantityArray (l:list) = [(l,length (takeWhile (\x->x==l) list)+1)]++(quantityArray (drop (length (takeWhile (\x->x==l) list)) list) )

longr::Eq a => [a] -> (a, Int)
longr array = foldl(\acc x-> if snd acc < snd x then x else acc) (head (quantityArray array)) (quantityArray array)