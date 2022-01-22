import System.IO
import System.Environment

type NodeId = Int
type Node = (NodeId, [NodeNeighbor])
type Graph = [Node]
type NodeNeighbor = (NodeId,String)

type Graph2 = [Node2]
type Node2 = ([NodeId2], [NodeNeighbor2])
type NodeId2 = Int
type NodeNeighbor2 = ([Int], String)

readFileToList :: Handle -> IO [String]
readFileToList fileHandle = do
   eof <- hIsEOF fileHandle
   if eof then return []
          else do line <- hGetLine fileHandle
                  rest <- readFileToList fileHandle
                  return (line:rest)

convertData:: [String]->Graph
convertData [] = []
convertData (a:b:rest) = (read a ::Int, (parseTuple.parseNumber) b):convertData rest
        where
                parseNumber:: String-> [String]
                parseNumber (w:word) = foldl(\acc x -> if x==' ' then acc++[""] else init acc++[last acc++[x]]) [[w]] word

                parseTuple:: [String]->[NodeNeighbor]
                parseTuple [] = []
                parseTuple (string:int:rest) = (read int :: Int,string): parseTuple rest

generateSubsets:: Graph->[[Int]]
generateSubsets graph= filter (\x-> length x >1)(subsets (listOfvertex graph))
        where
                listOfvertex :: Graph -> [Int]
                listOfvertex []=[]
                listOfvertex ((nodeId,neighbors):rest) = nodeId:listOfvertex rest

                subsets :: [Int] -> [[Int]]
                subsets [] = [[]]
                subsets (x : xs) =  subsets xs ++ map (x :) (subsets xs)

removeDubs:: [Int]->[Int]
removeDubs []=[]
removeDubs (l:list) = l:(removeDubs (filter (/=l) list))

generateRestGraph:: [[Int]]->Graph->[String]->Graph2
generateRestGraph [] _ _ =[]
generateRestGraph (s:subsets) graph alphabet = (s,generateNeighbor s graph alphabet):(generateRestGraph subsets graph alphabet)

parseAlphabet:: String->[String]
parseAlphabet []= []
parseAlphabet (w:word)= if w==' ' then parseAlphabet word else [w]:parseAlphabet word

anyRouteToZero:: Graph -> Bool
anyRouteToZero = (>0).length.checkAnyPathZero1
        where
                checkAnyPathZero1:: Graph ->[NodeNeighbor]
                checkAnyPathZero1 []=[]
                checkAnyPathZero1 ((nodeId,neighbors):rest) = filter (\x-> fst x== 0) neighbors ++ checkAnyPathZero1 rest

anySingleState:: Graph -> Bool
anySingleState [] = False
anySingleState ((nodeId,neighbors):rest) = not (any (\(x,y)->x/=nodeId) neighbors) || anySingleState rest

anySingleStateinGraph2:: Graph2 -> Bool
anySingleStateinGraph2 [] = False
anySingleStateinGraph2 ((nodeId,neighbors):rest) = if  length (filter(\(x,y)->length x ==1) neighbors) > 0 then True else anySingleStateinGraph2 rest

generateNeighbor:: [Int]->Graph->[String]->[NodeNeighbor2]
generateNeighbor _ _ [] = []
generateNeighbor x graph (a:alphabet) = (getSortedNeighbours x graph a,a):generateNeighbor x graph alphabet
        where
                getSortedNeighbours:: [Int]->Graph->String->[Int]
                getSortedNeighbours x graph letter = removeDubs (qsort (getNeighborbyLetter x graph letter))

                getNeighborbyLetter:: [Int]->Graph->String->[Int]
                getNeighborbyLetter [] _ _=[]
                getNeighborbyLetter (x:xs) graph letter= if((getIdNeighbour x graph letter)==(-1)) then getNeighborbyLetter xs graph letter else (getIdNeighbour x graph letter): (getNeighborbyLetter xs graph letter)

                getIdNeighbour:: Int -> Graph-> String -> NodeId
                getIdNeighbour number graph letter = if( length (getNodeNeighborSelectedLetter number graph letter)>0) then fst (head(getNodeNeighborSelectedLetter number graph letter)) else -1

                getNodeNeighborSelectedLetter:: Int -> Graph-> String -> [(NodeId, String)]
                getNodeNeighborSelectedLetter number graph letter =  filter (\(neighbor,sign)-> letter==sign) (getNodeNeighbor number graph)

                getNodeNeighbor:: Int -> Graph -> [NodeNeighbor]
                getNodeNeighbor _ [] = []
                getNodeNeighbor number ((nodeId,neighbors):rest) = if nodeId == number then neighbors else getNodeNeighbor number rest

printGentle::Graph2->IO()
printGentle [] = return ()
printGentle (x:rest)= do
                        print x
                        printGentle rest

printGentleData:: Graph->IO()
printGentleData [] = return ()
printGentleData (node:rest) = do
                                print node
                                printGentleData rest

main :: IO ()
main = do
        (inFileName:_) <-getArgs
        inFileHandle <-openFile inFileName ReadMode
        list<-readFileToList inFileHandle
        print list
        let alphabet = (parseAlphabet.head) list
        print alphabet
        let graph = (convertData.tail) list
        printGentleData graph
        let graph2 = generateRestGraph (generateSubsets graph) graph alphabet
        printGentle graph2
        hClose inFileHandle
                                
qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = (qsort left) ++ [x]++(qsort right)
        where
                left = [y|y<-xs,y<=x]
                right = [y|y<-xs,y>x]