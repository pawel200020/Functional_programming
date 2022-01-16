f:: [Int]->Int
f[]=1
f list = (+) (head list) 1 `div` (f.tail) list