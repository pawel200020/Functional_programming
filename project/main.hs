import System.IO
import System.Environment

readFileToList :: Handle -> IO [Int]
readFileToList fileHandle = do
   eof <- hIsEOF fileHandle
   if eof then return []
          else do line <- hGetLine fileHandle
                  rest <- readFileToList fileHandle
                  let num = read line ::Int
                  return (num:rest)

convertData:: Int->[Int]->[([Int],[[Int]])]
convertData count (l:list) = foldl (\acc x -> if length (snd (last acc)) == count  then acc++[([x],[])] else init acc++[(fst (last acc), snd (last acc)++[[x]])]) [([l],[])] list

subsets :: [Int] -> [[Int]]
subsets [] = [[]]
subsets (x : xs) =  subsets xs ++ map (x :) (subsets xs)

showList2 :: [([Int],[[Int]])] -> IO ()
showList2 [] = return ()
showList2 (x:xs) = do           print x
                                showList2 xs

takeFirst::[([Int],[[Int]])]->[Int]
takeFirst  = foldr ((++) . fst) []

showList3 :: [[Int]] -> IO ()
showList3 [] = return ()
showList3 (x:xs) = do           print x
                                showList3 xs

listMerger :: [[Int]]->[[Int]]->[[Int]]
listMerger [][] = []
listMerger [] y = y
listMerger x [] = x
listMerger (x:xs) (y:ys)= if(x<y) then (x++y) : listMerger xs ys else (y++x) : listMerger xs ys

getNeighbours :: Int->[([Int],[[Int]])] ->[[Int]]
getNeighbours num list = snd (head (filter (\x-> fst x== [num]) list))

getNeighbours2 :: [Int]->[([Int],[[Int]])] ->([Int],[[Int]])
getNeighbours2 num list = (head (filter (\x-> fst x== num) list))

findElem:: [Int]->Int-> Bool
findElem list item = length (filter (\x->x==item) list)>0

removeDubs :: [Int] -> [Int]
removeDubs  = foldl (\acc x -> if not (findElem acc x)  then acc++[x] else acc) []

convert2 :: [Int]->[([Int],[[Int]])]->[[Int]]
convert2 (l:list) graph= foldl(\acc x->(listMerger (getNeighbours x graph) acc)) (getNeighbours l graph) list

finalconvert2::[([Int],[[Int]])]->[([Int],[[Int]])]
finalconvert2 graph = tail (foldl (\acc x->acc++[(x, map removeDubs (convert2 x graph))]) [([],[[]])] (filter (\x-> length x>1)(subsets (takeFirst graph))))

-- isOnList:: [[Int]]->[Int]->Bool
-- isOnList [] _ = False
-- isOnList (l:list) item = (l == item) || isOnList list item

--checkHim:: [([Int],[[Int]])]->([Int],[[Int]]) -> [[Int]]->[[Int]]

main = do
        (inFileName:_) <-getArgs
        inFileHandle <-openFile inFileName ReadMode
        list<-readFileToList inFileHandle
        let list2 = convertData (head list) (tail list)
        let graph = list2 ++ finalconvert2 list2
        showList2 graph
        hClose inFileHandle