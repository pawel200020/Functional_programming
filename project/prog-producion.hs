import System.IO
import System.Environment
--Najkrótsze słowo synchronizujące.
--pogram pobiera z pliku opis deterministycznego automatu następnie stwierdza czy ma on słowo synchronizujące czy nie
--słowo synchronizujące to takie słowo które niezależnie od stanu w którym się znajdujemy to zawsze skończymy w tym samym stanie
--źródło algorytmu: http://www.math.uni.wroc.pl/~kisiel/auto/volkov-surv.pdf

--pierwszy typ:
--graf składa się z tablicy węzłów,
--węzeł składa się ze swojego id (INT) oraz listy sąsaiadów
--sąsiad z koleji to jego id (INT) oraz string literka pod której wpływem możemy do niego przejść

-- ten typ jest potrzebny na początku programu sparsować do niego graf wczytywany z pliku
type NodeId = Int
type Node = (NodeId, [NodeNeighbor])
type Graph = [Node]
type NodeNeighbor = (NodeId,String)

--drugi typ:
-- graf2 składa się z tablicy węzłów2,
--węzeł2 składa się z tablicy NodeId2 która jest tablicą intów, oraz tablicy sąsiadów2
--sąsiad składa się ze swojego id w postaci tablicy intów oraz stringa czyli literki pod której wpływem przechodzimy do niego

--ten typ jest potrzebny do zasadniczego algorytmu, graf2 powstaje w wyniku konstrukcji podzbiorów grafu1.
type Graph2 = [Node2]
type Node2 = ([NodeId2], [NodeNeighbor2])
type NodeId2 = Int
type NodeNeighbor2 = ([NodeId2], String)

--klasyczne wczytanie danych z pliku do tablicy stringów
readFileToList :: Handle -> IO [String]
readFileToList fileHandle = do
   eof <- hIsEOF fileHandle
   if eof then return []
          else do line <- hGetLine fileHandle
                  rest <- readFileToList fileHandle
                  return (line:rest)

--konwersja surowej listy stringów do grafu pierwszy jest numer stanu (a) natomiast reszta to kolejno literka i numer stanu do którego przechodzimy pod jej wpływem przechodzimy
convertData:: [String]->Graph
convertData [] = []
convertData (a:b:rest) = (read a ::Int, (parseTuple.parseNumber) b):convertData rest
        where
                --parseNumber rozdzielenie wiersza typu ["a 0 b 1"] do ["a","0","b","1"]
                parseNumber:: String-> [String]
                --jeśli dostaniesz spację to dodaj nowy rekord do listy jeśli nie to do ostatniego elementu dodaj to co masz
                parseNumber (w:word) = foldl(\acc x -> if x==' ' then acc++[""] else init acc++[last acc++[x]]) [[w]] word
                
                --parseTuple konwertuje listę otrzymaną od parseNumber do formatu "sąsiada" ["a","0","b","1"] -> [(0,"a"),(1,"b")]
                parseTuple:: [String]->[NodeNeighbor]
                parseTuple [] = []
                parseTuple (string:int:rest) = (read int :: Int,string): parseTuple rest 

--generateSubsets generuje listę podzbiorów zbioru stanów wierzchołków grafu o długości większej niż 1 gdyż mniejsze nie będą nam potrzebne w algorytmie ponieważ jedyne co nas interesuje to czy jest droga do singletonu.
generateSubsets:: Graph->[[Int]]
generateSubsets graph= filter (\x-> length x >1)(subsets (listOfvertex graph))
        where

                --pobranie listy id-ów wszystkich wierzchołków z grafu
                listOfvertex :: Graph -> [Int]
                listOfvertex []=[]
                listOfvertex ((nodeId,neighbors):rest) = nodeId:listOfvertex rest

                --klasyczne generowanie podzbiorów po przez dołączanie do poprzedniej listy nowej zmapowanej o kolejny element
                subsets :: [Int] -> [[Int]]
                subsets [] = [[]]
                subsets (x : xs) =  subsets xs ++ map (x :) (subsets xs)

--removeDubs - usuwanie duplikatów z listy
removeDubs:: [Int]->[Int]
removeDubs []=[]
removeDubs (l:list) = l:(removeDubs (filter (/=l) list))

--sprawdzamy czy lista ma zadany element
hasElement :: Eq a => [a] -> a -> Bool
hasElement lst a =
  case filtered_list of
    [] -> False
    _ -> True
    where
        filtered_list = filter ((==) a) lst

--funkcja generująca graf2 z listy podzbiorów stanów, grafu 1 oraz alfabetu
--bierzemy 1-szy podzbiór z listy i wiedząc że node w grafie 2 to (Node2 = ([NodeId2], [NodeNeighbor2])) na pierwsze miejsce pary dajemy nasz podzbiór natomiast następnie generujmey sąsiadów funkcją generateNeighbor od naszego podzbioru stanów, grafu1 oraz przefiltrowanego alfabetu
generateRestGraph:: [[Int]]->Graph->[String]->Graph2
generateRestGraph [] _ _ =[]
generateRestGraph (s:subsets) graph alphabet = (s,generateNeighbor s graph (sAlphabet graph alphabet s)):(generateRestGraph subsets graph alphabet)
          where
              -- dla grafu, alfabetu i konkretnego zbioru stanów, jaki jest "podalfabet", którego
              -- litery mogą być wybrane w każdym z tych stanów?
              --filtrujemy alfabet do liter tylko takich które możemy wybrać w każdym ze stanów
              sAlphabet :: Graph -> [String] -> [Int] -> [String]
              sAlphabet graph alphabet s = filter (checkLetter graph s) alphabet              

              -- czy dana litera, dla danego grafu i danego zbioru stanów
              -- może być wybrana w każdym z tych stanów?
              --graf1, podzbiór stanów, literka
              checkLetter :: Graph -> [Int] -> String -> Bool
              checkLetter graph s letter =
                case graph of
                  [] -> True    --pusta lista to znaczy że jest ok
                  (node:rest) ->
                    let (node_state, nbrs) = node
                    in
                      if (hasElement s node_state) -- jeśli w naszej liście jest ten stan który obecnie pobraliśmy z grafu
                        then
                          if (hasLetter nbrs letter)        --to sprawdzamy czy możemy przejść pod wpływem danej literki
                            then checkLetter rest s letter  --jeśli tak jest ok i sprawdzamy kolejne stany
                            else False                      --jeśli nie to nie da się przejść z tego stanu pod wpływem tej literki to znaczy że nie będziemy jej dodać gdyż nie jest osiągalna ze wszystkich stanów
                        else checkLetter rest s letter      --jeśli akurat ten węzeł grafu nie jest w naszym kręgu zainteresowań sprawdzamy kolejny
              
              --czy dany stan zawiera w swoich przejściach literkę
              hasLetter :: [NodeNeighbor] -> String -> Bool
              hasLetter nbrs letter =
                case nbrs of
                  [] -> False       --jeśli pusto u sąsiadów to fałsz 
                  (n:rest) ->
                    let the_letter = snd n      --bierzemy drugi element sąsiada - to literka
                    in
                      if the_letter == letter     --jeśli literka się zgdza z naszą to znaczy że można pod wpływem jej do gdzieś prześć
                        then True                 --wtedy prawda
                        else hasLetter rest letter  --jeszcze nie skończyliśmy dlatego idziemy dalej i sprawdzamy kolejnych sąsiadów
              
--funkcja parsująca alfabet ["a b c"] na ["a", "b", "c"]
parseAlphabet:: String->[String]
parseAlphabet []= []
parseAlphabet (w:word)= if w==' ' then parseAlphabet word else [w]:parseAlphabet word

--czy jest jakikolwiek pojedyczny stan w grafie2?
anySingleStateinGraph2:: Graph2 -> Bool
anySingleStateinGraph2 [] = False --pusty graf i nic nie znaleziono to fałsz
anySingleStateinGraph2 ((nodeId,neighbors):rest) =
  if  length (filter(\(x,y)->length x ==1) neighbors) > 0 then True else anySingleStateinGraph2 rest -- jeśli wśród sąsiadów będzie taki którego długość jest 1 to mamy singleton i automat może lecz nie musi mieć słowo synchronizujące

--generowanie sąsiadów dla podzbioru stanów, uwaga lista alfabetu musi być wyfiltrowana, ze względu na osiągalność ze wszystkich stanów
generateNeighbor:: [Int]->Graph->[String]->[NodeNeighbor2]
generateNeighbor _ _ [] = []
generateNeighbor x graph (a:alphabet) = (getSortedNeighbours x graph a,a):generateNeighbor x graph alphabet --na początku bierzemy 1 szą literkę i patrzymy gdzie możemy iść pod jej wpływem z każdego stanu ze zbioru x
        where
                --funkcja zwraca podzbiór stanów do których możemy przejść pod wpływem danej literki i zadanego podzbioru zbioru stanów, jest to podzbiór uporządkowany rosnąco i niezawierający duplikatów
                getSortedNeighbours:: [Int]->Graph->String->[Int]
                getSortedNeighbours x graph letter = removeDubs (qsort (getNeighborbyLetter x graph letter))

                --funkcja zwraca podzbiór stanów do których możemy przejść pod wpływem danej literki i zadanego podzbioru zbioru stanów, tutaj jest nam potrzebna gwarancja że aby napewno z każdego stanu możemy przejść pod wpływem danej literki
                getNeighborbyLetter:: [Int]->Graph->String->[Int]
                getNeighborbyLetter [] _ _=[]
                getNeighborbyLetter (x:xs) graph letter= (getIdNeighbour x graph letter): (getNeighborbyLetter xs graph letter)

                --funkcja zwracająca id sąsiada dla zadanego stanu grafu i literki, jeśli go nie ma zwraca -1 które akurat w naszym zadaniu nigdy nie nastąpi ponieważ sprawdzamy wcześniej czy aby napewno można przejść pod wpływem danej literki z danego stanu.
                getIdNeighbour:: Int -> Graph-> String -> NodeId
                getIdNeighbour number graph letter = if( length (getNodeNeighborSelectedLetter number graph letter)>0) then fst (head(getNodeNeighborSelectedLetter number graph letter)) else -1

                --szukamy sąsiada wierzchołka po zadanej literce
                getNodeNeighborSelectedLetter:: Int -> Graph-> String -> [(NodeId, String)]
                getNodeNeighborSelectedLetter number graph letter =  filter (\(neighbor,sign)-> letter==sign) (getNodeNeighbor number graph)

                --szukamy sąsiadów zadanego wierzchołka i zwracamy ich listę
                getNodeNeighbor:: Int -> Graph -> [NodeNeighbor]
                getNodeNeighbor _ [] = []
                getNodeNeighbor number ((nodeId,neighbors):rest) = if nodeId == number then neighbors else getNodeNeighbor number rest

--"ładne" wypisanie grafu2
printGentle::Graph2->IO()
printGentle [] = return ()
printGentle (x:rest)= do
                        print x
                        printGentle rest
--"ładne" wypisanie grafu
printGentleData:: Graph->IO()
printGentleData [] = return ()
printGentleData (node:rest) = do
                                print node
                                printGentleData rest

--algorytm BFS
--Nowy typ BFSNode - składa się z wierzchołka grafu2 (Node2) oraz stringa - dotychczasowego słowa synchronizującego
type BFSNode = (Node2, String)
--Funkcja przyjmuje graf2 oraz w zależności od tego czy znajdzie słowo synchronizujące czy nie
--jeśli nie zwróci Nothing
--jeśli znajdzie to zwróci to słowo
--maybe jest podobne do nulla w programowaniu imperatywnym
mbSyncWord :: Graph2 -> Maybe String
mbSyncWord graph =
  if anySingleStateinGraph2 graph --jeśli nie ma żandego singletona w naszym grafie2 to w zasadzie nie ma co szukać
    then
      --start przyjmuje "najdłuższy stan" w grafie 2, to od niego zaczynamy przeszukiwanie bfs zgodnie z algorytmem
      let start = longestState graph
      --rozpoczynamy przeszukiwanie bfs funkcja map jest swego rodzaju zabezpieczeniem gdybyśmy mieli więcej niż jeden stan początkowy tzn ten który zawiera wszystkie stany automatu początkowego.
      in  bfsSearch graph [] $ map (\n -> (n, "")) start
    
    else -- dlatego zwracamy nothing
      Nothing

--longestState znajduje najdłuższy stan / stany w grafie.
longestState :: Graph2 -> [Node2]
longestState graph =
  -- pobieramy tylko pierwsze elementy grafu tzn wierzchołki bez sąsiadów
  -- do lens przypisujemy listę ich długości
  -- natomiast do max_len zwraca długość najdłuższego wierzchołka
  let nodes = map fst graph
      lens = map length nodes
      max_len = foldl max 0 lens
  --przefiltrowanie po grafie wierzchołków o tej długości da nam porządany wierzchołek/ki 
  in  filter (\node -> length (fst node) == max_len) graph


--main BfsSearch przyjmuje nasz graf listę "zabronionych wierzchołków" tzn tych które już odwiedziliśmy aby się nie zapętlić, wierzchołki obecnie badane kolejne dzieci są odkładane na koniec listy, zwraca natomiast albo nic (Nothing) jeśli przeszliśmy cały graf i nie znaleźliśmy singletona, albo słowo synchronizujące
bfsSearch :: Graph2 -> [[NodeId2]] -> [BFSNode] -> Maybe String
bfsSearch graph tabu_node_states lst =
  case lst of
    [] -> Nothing --pusto, przeszliśmy wszystko, nic nie znaleźliśmy
    (bfs_node : rest) ->   --bierzemy 1 szy z listy
      let
        (node, current_word) = bfs_node --(Node2,String słowo)
        -- interesuje nas lista stanów oraz lista możliwych przejść
        (node_states, moves) = node  --([Node2Id],[NodeNeigbors2])
        -- jeśli na liście możliwych przejść jest jakiś singleton, to koniec i sukces
        -- w przeciwnym przypadku trzeba dodać kolejne możliwości do listy
      in
        if hasElement tabu_node_states node_states
          then
            -- jeśli węzeł był już sprawdzany i jest na tzw "liście tabu" to ignorujemy i lecimy z resztą
            bfsSearch graph tabu_node_states rest
          else
            case singletonWord moves of   --jeśli mamy singleton w sąsiadach badanego wierzchołka znaleźliśmy słowo synchronizujące
              Just w -> Just $ current_word ++ w 
              Nothing -> 
                --jeśli nie no to nowe wierzchołki do zbadania tworzymy mapując sąsiadów tego wierzchołka do typu BFSNode i dorzucamy do tabu obecnie badany wierzchołek
                let new_nodes = map (nodeToCheck graph current_word) moves
                    new_tabu = node_states : tabu_node_states
                in  bfsSearch graph new_tabu $ rest ++ new_nodes

--funkcja mapująca sąsiada wierzchołka do bfsNode, przyjmuje graf2, dotychczasowe słowo i sąsiada składającego się ze swojego id oraz literki po której do niego przechodzimy
nodeToCheck :: Graph2 -> String -> NodeNeighbor2 -> BFSNode
nodeToCheck graph word (pth_label, new_letter) = (graph_node, new_word)
  where
    new_word = word ++ new_letter
    graph_node = head $ filter (\n -> (fst n) == pth_label) graph --pobieramy node2 z grafu właściwego

--sprawdź czy jest singleton na liście sąsiadów
singletonWord :: [NodeNeighbor2] -> Maybe String
singletonWord lst =
  case lst of
    [] -> Nothing -- pusta lista to nie ma
    (currentNode:rest) ->
      let (nodeID, letter) = currentNode
      in
        case nodeID of
          [_] -> Just letter --jeśli masz singletona to znalazłeś słowo synchronizujące i zwróć tę literkę
          _ -> singletonWord rest  --jeśli nie to szukaj dalej

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
