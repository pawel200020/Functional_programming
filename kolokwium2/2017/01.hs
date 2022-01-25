prefix:: [a] ->[[a]]
prefix  = foldl(\acc x -> acc++[(last acc)++[x]]) [[]]

ps :: [a] ->[[a]]
ps  = prefix


prefixes :: Eq a=> [a] -> [[a]]
prefixes list = foldr (\el acc -> [] : map (\x->el:x) acc) [[]] list