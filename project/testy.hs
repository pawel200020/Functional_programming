parseNumber:: String-> [String]
parseNumber (w:word) = foldl(\acc x -> if x==' ' then acc++[""] else init acc++[last acc++[x]]) [[w]] word

parseTuple:: [String]->[(Int,String)]
parseTuple [] = []
parseTuple (string:int:rest) = (read int :: Int,string): parseTuple rest

subsets :: [Int] -> [[Int]]
subsets [] = [[]]
subsets (x : xs) =  subsets xs ++ map (x :) (subsets xs)