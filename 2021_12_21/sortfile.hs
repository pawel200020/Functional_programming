import System.IO
import System.Environment

readFileToList2 :: Handle -> IO [String]
readFileToList2 fileHandle = do
   eof <- hIsEOF fileHandle
   if eof then return []
          else do line <- hGetLine fileHandle
                  rest <- readFileToList2 fileHandle
                  return (line:rest)


-- Funkcja wczytuje linijka po linijce i wynik opakowuje w monade IO                
readFileToList :: Handle -> IO [Integer]
readFileToList fileHandle = do
   eof <- hIsEOF fileHandle
   if eof then return []
          else do line <- hGetLine fileHandle
                  rest <- readFileToList fileHandle
                  let num = read line ::Integer
                  return (num:rest)

-- Funkcja zapisujaca liste do pliku
writeListToFile :: [Integer] -> Handle->IO ()
writeListToFile [] _ = return ()
writeListToFile (l:list) handle = do
    hPutStrLn handle (show l)
    writeListToFile list handle

qsort :: [Integer] -> [Integer]
qsort [] = []
qsort (x:xs) = (qsort left) ++ [x]++(qsort right)
                where 
		            left = [y|y<-xs,y<=x]
		            right = [y|y<-xs,y>x]

main = do
          (inFileName:outFileName:rest) <-getArgs
          inFileHandle <-openFile inFileName ReadMode
          outFileHandle <-openFile outFileName WriteMode
          list<-readFileToList inFileHandle
          writeListToFile (qsort list) outFileHandle
          hClose inFileHandle
          hClose outFileHandle