prefix:: [a] ->[[a]]
prefix  = foldl(\acc x -> acc++[(last acc)++[x]]) [[]]

ps :: [a] ->[[a]]
ps  = prefix