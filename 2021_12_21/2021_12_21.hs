-- kwadraty:: Int->Int 
-- kwadraty a = foldl(\acc x->acc+x*x) 0 [1..a]

-- main :: IO ()
-- main = do 
--          print "Podaj liczbe: "
--          numStr <- getLine
--          let num = read numStr :: Int
--          print $ (kwadraty num)

-- displayList:: [Int]->IO()
-- displayList [] = return ()
-- displayList (a:array) = do
--                         putStrLn (show (a*a)) 
--                         displayList array
-- main :: IO ()
-- main = do 
--          print "Podaj liczbe: "
--          numStr <- getLine
--          let num = read numStr :: Int
--          displayList [1..num]

-- import System.IO
-- import System.Environment

-- countLines :: Handle -> Int ->IO Int
-- countLines handle i = do
-- 		      eof<-hIsEOF handle
-- 		      if eof then return i 
-- 			     else 
-- 				  do
-- 				    line<-hGetLine handle
-- 				    countLines handle (i+1)
-- main :: IO ()        
-- main = do
--            (firstArg:_) <-getArgs
--            fileHandle <-openFile firstArg ReadMode
--            toPrint <- (countLines fileHandle 0)
--            print toPrint
--            hClose fileHandle   
-- import System.IO;
-- import System.Environment;

-- writeToFile :: Handle -> [String] -> IO ()
-- writeToFile handle [] = return ()
-- writeToFile handle (x:xs) = 
--             do
--             hPutStrLn handle x
--             writeToFile handle xs
            

-- main :: IO ()        
-- main = do
--         (firstArg:secondArg:_) <- getArgs
--         fileHandle <-openFile secondArg WriteMode
--         writeToFile fileHandle [x | x<-generateWords (read firstArg) , isPalin x]
--         hClose fileHandle      

-- generateWords:: Int->[String]
-- generateWords 0=[]
-- generateWords 1=["a","b"]
-- generateWords x=(map (\x->'a':x) (generateWords (x-1)))++(map (\x->'b':x) (generateWords (x-1)))

-- isPalin::String->Bool
-- isPalin x=x==(reverse x)
import System.IO
isums :: IO Int
isums = do
    num <- readLn
    if num == 0
        then return 0
        else 
            do
                sum <- isums 
                return (num + sum)

main :: IO ()
main = do 
        toPrint <- isums
        print toPrint