data Tree a = Empty | Node a (Tree a) (Tree a)

isEmpty :: Tree a -> Bool
isEmpty Empty = True 
isEmpty _ = False

size :: Tree a -> Int
size Empty = 0
size (Node-left rigt)= 1+ size left +size right

depth :: Tree a -> Int
depth Empty = 0
depth a = 1 + max depth left depth right
--dlaczego algebraic data types - skąd ta nazwa