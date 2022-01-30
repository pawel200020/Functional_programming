import Debug.Trace
import System.IO
import System.Environment


type NodeId = Int
type Node = (NodeId, [NodeNeighbor])
type Graph = [Node]
type NodeNeighbor = (NodeId,String)

type Graph2 = [Node2]
type Node2 = ([NodeId2], [NodeNeighbor2])
type NodeId2 = Int
type NodeNeighbor2 = ([NodeId2], String)

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

hasElement :: Eq a => [a] -> a -> Bool
hasElement lst a =
  case filtered_list of
    [] -> False
    _ -> True

  where
    filtered_list = filter ((==) a) lst
    -- (\x -> a == x)

generateRestGraph:: [[Int]]->Graph->[String]->Graph2
generateRestGraph [] _ _ =[]
generateRestGraph (s:subsets) graph alphabet = (s,generateNeighbor s graph (sAlphabet graph alphabet s)):(generateRestGraph subsets graph alphabet)
  where
    -- dla grafu, alfabetu i konkretnego zbioru stanów, jaki jest "podalfabet", którego
    -- litery mogą być wybrane w każdym z tych stanów?
    sAlphabet :: Graph -> [String] -> [Int] -> [String]
    sAlphabet graph alphabet s = filter (checkLetter graph s) alphabet

    hasLetter :: [NodeNeighbor] -> String -> Bool
    hasLetter nbrs letter =
      case nbrs of
        [] -> False
        (n:rest) ->
          let the_letter = snd n
          in
            if the_letter == letter
              then True
              else hasLetter rest letter
          --case n of
          --  (_, letter) -> True
          --  _ -> hasLetter rest letter

    -- czy dana litera, dla danego grafu i danego zbioru stanów
    -- może być wybrana w każdym z tych stanów?
    checkLetter :: Graph -> [Int] -> String -> Bool
    checkLetter graph s letter =
      case graph of
        [] -> True
        (node:rest) ->
          let (node_state, nbrs) = node
          in
            if (hasElement s node_state)
              then
                if (hasLetter nbrs letter)
                  then checkLetter rest s letter
                  else False
              else checkLetter rest s letter
              

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
anySingleStateinGraph2 ((nodeId,neighbors):rest) =
  if  length (filter(\(x,y)->length x ==1) neighbors) > 0 then True else anySingleStateinGraph2 rest

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

-- BFS (?)

type BFSNode = (Node2, String)

mbSyncWord :: Graph2 -> Maybe String
mbSyncWord graph =
  let t = anySingleStateinGraph2 graph
  in
    if traceShow t t
      then
        -- find the "longest" state
        let s = longestState graph
            start = trace ("The longest:" ++ (show s)) s
        -- add the found node to the list
        -- start the list analysis
        in  bfsSearch graph [] $ map (\n -> (n, "")) start
      
      else
        trace "Nic z tego" Nothing

longestState :: Graph2 -> [Node2]
longestState graph =
  -- Node2 -> długość
  let nodes = map fst graph
      lens = map length nodes
  -- max z listy długości
      max_len = foldl max 0 lens
  -- filter po tej najdłuższej długości
  --   i to będzie wynik
  in  filter (\node -> length (fst node) == max_len) graph

bfsSearch :: Graph2 -> [[NodeId2]] -> [BFSNode] -> Maybe String
bfsSearch graph tabu_node_states lst =
  case lst of
    [] -> Nothing
    (bfs_node : rest) ->
      let
        (node, current_word) = trace ("bfs node:" ++ (show bfs_node)) bfs_node
        -- interesuje nas lista stanów oraz lista możliwych przejść
        (node_states, moves) = node
        _ = trace ("Checking moves: " ++ (show moves)) moves
        -- jeśli na tej liście jest jakiś singleton, to koniec i sukces
        -- w przeciwnym przypadku trzeba dodać kolejne możliwości do listy
      in
        if hasElement tabu_node_states node_states
          then
            -- ignorujemy ten węzeł - już był sprawdzony wcześniej
            bfsSearch graph tabu_node_states rest
          else
            case singletonWord moves of
              Just w -> Just $ current_word ++ w
              Nothing ->
                let new_nodes = map (nodeToCheck graph current_word) moves
                    new_tabu = node_states : tabu_node_states
                in  bfsSearch graph new_tabu $ rest ++ new_nodes

nodeToCheck :: Graph2 -> String -> NodeNeighbor2 -> BFSNode
nodeToCheck graph word (pth_label, new_letter) = (graph_node, new_word)
  where
    new_word = word ++ new_letter
    graph_node = head $ filter (\n -> (fst n) == pth_label) graph

singletonWord :: [NodeNeighbor2] -> Maybe String
singletonWord lst =
  case lst of
    [] -> Nothing
    (nn:rest) ->
      let (l, w) = nn
      in
        case l of
          [_] -> Just w
          _ -> singletonWord rest

-- the program

main :: IO ()
main = do
        (inFileName:_) <-getArgs                        --pobranie nazwy pliku jako argument
        inFileHandle <-openFile inFileName ReadMode     --otwarcie pliku, "uzyskanie uchwytu"
        list<-readFileToList inFileHandle               --wczytanie danych z pliku do listy stringów
        --print list                                    --opcjonalne wypisanie "surowych danych z pliku"                   
        let alphabet = (parseAlphabet.head) list        -- w głowie listy znajduje się alfabet, parsuję go do listy stringów ["a","b",...]
        --print alphabet                                --opcjonalne wypisanie zparsowanego alfabetu
        let graph = (convertData.tail) list             --konwersja reszty danych na graf 
        printGentleData graph                           --opcjonalne wypisanie zparsowanych danych które reprezentują nasz graf
        let graph2 = generateRestGraph (generateSubsets graph) graph alphabet     --generuję drugi graf konstrukcją podzbiorów tzn generuję wszystkie podzbiory zbioru wierzchołków grafu 1 szego a potem generuję dla nich sąsiadów na podstawie grafu 1 i alfabetu
        print "Graf2:"
        printGentle graph2                              --opcjonalne wypisanie grafu2
        --przechodzimy do najważnieszego, funkcja mbSyncWord zwaraca Nothing jeśli algorytm bfs nie znalazł ścieżki od stanu zawierającego numery wszystkich stanów w rozszeżonym grafie do singletonu, natomiast jeśli znalazł taką ścieżkę to dostajemy najkrótsze słowo synchronizujące. Najkrótsze dlatego, że bfs znajduje zawsze najkrótszą ścieżkę.
        let sync_word = mbSyncWord graph2            
        case sync_word of                             --prosty case sprawdzający co zwraca mbSyncWord
          Nothing ->
            print "Automaton is not synchronizing. Sorry."

          Just word ->
            print $ "Automaton is synchronizing: " ++ word
--quick sort który wykorzystuję przy sortowaniu podzbiorów stanów które wygenerowałem                           
qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = (qsort left) ++ [x]++(qsort right)
        where
                left = [y|y<-xs,y<=x]
                right = [y|y<-xs,y>x]
