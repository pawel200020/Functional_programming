data Tree a = Empty | Node a (Tree a) (Tree a)

hasElement:: Eq a => a -> Tree a -> Bool
hasElement _ Empty = False 
hasElement x (Node a Empty Empty) = if x==a then True  else False 
hasElement x (Node a l r) = if x==a then True else (hasElement x l) || (hasElement x r)

findPath :: Eq a => a -> Tree a -> [a]
findPath _ Empty = []
findPath x (Node a l r) = if hasElement x (Node a l r) then [a]++(findPath x l) ++ (findPath x r)else (findPath x l) ++ (findPath x r)