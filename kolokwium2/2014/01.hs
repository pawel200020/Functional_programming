f:: [Int]->[[Int]]
f (a:array) = foldl(\acc x -> if (last.last) acc + 1 == x then init acc++[last acc++[x]] else acc++[[x]]) [[a]] array