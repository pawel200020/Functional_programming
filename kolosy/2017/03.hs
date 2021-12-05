data Sdb a = Solo a | Node1 a  (Sdb a) | Node2 a  (Sdb a) (Sdb a)

el :: Eq a => Sdb a -> a -> Bool
el (Solo a) em = a == em
el (Node1 a  subnode) em = (a == em) || el subnode em
el (Node2 a  subnode1 subnode2) em = (a == em) || el subnode1 em || el subnode2 em


eq :: Eq a => Sdb a -> Sdb a -> Bool
eq (Solo a) (Solo b) = a == b
eq (Node1 a  subnode) (Node1 b subnode2)  = (a == b) && eq subnode subnode2
eq (Node2 a  subnode1 subnode2) (Node2 b  subnode3 subnode4) = (a==b) && ((eq subnode1 subnode3 && eq subnode2 subnode4) ||(eq subnode1 subnode4 && eq subnode2 subnode3))
eq _ _ = False

sdb2list :: Sdb a -> [a]
sdb2list (Solo a) = [a]
sdb2list  (Node1 a  subnode) = a : sdb2list subnode
sdb2list  (Node2 a  subnode1 subnode2) = a : sdb2list subnode1 ++ sdb2list subnode2
