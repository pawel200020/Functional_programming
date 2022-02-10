
countWords:: String-> [(Char,Int)]
countWords [] = []
countWords (w:word) = (w, length (filter(==w) word)+1):countWords (filter(/=w) word)

readUntilDot ::IO()
readUntilDot = do
    nStr <- getLine
    if nStr  =="." then return () else do
        print (countWords nStr)
        readUntilDot


main :: IO ()
main = do 
         print "podaj n: "
         num <-  getLine
         let num2 = read num ::Int
         print num2
         readUntilDot