f list = filter (\x->x>5)
f1 = filter (\x-> (>) x 5)
f2 = filter (>5)
----------------------
g list = map (\x->x/5) list
g1 = map (\x-> (/) x 5)
g2 = map(/5)
-----------------
nonZero::[Int]->Int
nonZero list = length (filter (\x->x/=0) list)

nonZero1::[Int]->Int
nonZero1 = length . filter (\x->x/=0)

nonZero2::[Int]->Int
nonZero2 = length . filter (/=0)
-------------------------------
m1 x list = map(\y->y/x) list
m2 x = map(\y->y/x)
m3 x = map(/x)
m31 x = (map.(/))x
m4 = map.(/)
-------------------------------
d::[Double]->Double->[Double]
d list x = map (\y->y/x) list

d1::[Double]->Double->[Double]
d1 list x = map (/x) list

d2::[Double]->Double->[Double]
d2 list x = flip g list x
            where g x list = map (/x) list

d3::[Double]->Double->[Double]
d3 = flip g
            where g x = map (/x)

d4::[Double]->Double->[Double]
d4 = flip g
            where g = map . (/)

d41::[Double]->Double->[Double]
d41= flip g
            where g = map .flip (/)

d5::[Double]->Double->[Double]
d5 =flip(map .flip (/))
---------------------------------
wiekszeOd  l a = [x | x<-l,x>a]

wiekszeOd1  l a = filter(\x->x>a) l

wiekszeOd2  l a = filter(>a) l

wiekszeOd3 :: Ord a => [a] -> a -> [a]
wiekszeOd3  l a = flip g l a
        where g a = filter (>a)

wiekszeOd4 :: Ord a => [a] -> a -> [a]
wiekszeOd4 = flip g
        where g  = filter .flip (>)

wiekszeOd5 :: Ord a => [a] -> a -> [a]
wiekszeOd5 = flip ( filter .flip (>))
-------------------------------------

foo::Ord a=>[(a,a)]->[(a,a)]
foo array =takeWhile (\(a,b)->a<b) array

foo2::Ord a=>[(a,a)]->[(a,a)]
foo2 =takeWhile (\(a,b)->a<b)

foo3::Ord a=>[(a,a)]->[(a,a)]
foo3 =takeWhile (uncurry (<))