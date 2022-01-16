readUntilDot ::IO()
readUntilDot = do
    nStr <- getLine
    if nStr  =="." then return () else do
        putStrLn (reverse nStr)
        readUntilDot


main :: IO ()
main = do 
         print "podaj n: "
         readUntilDot