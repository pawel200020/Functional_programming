data Tree a = Empty | Node a (Tree a) (Tree a)

isExist:: Eq a => a -> Tree a -> Bool
isExist _ Empty = False
isExist  x (Node a left right) = if(x== a) then True else isExist x left 
-- findPath :: Eq a => a -> Tree a -> [a]
-- findPath _ Empty  = []
-- findPath x Node a (Tree a) (Tree a) = if 