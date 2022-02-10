putSpace::Int->IO()
putSpace 0 = return ()
putSpace n = do
            putStr (" ")
            putSpace (n-1)

drawTree:: Int -> Int -> IO()
drawTree (0) _ = return()
drawTree s1 s2 = do
                putSpace (s1-1)
                putStr ("/")
                putSpace s2
                putStr("\\")
                putStrLn ("")
                drawTree (s1-1) (s2+2)

drawLow:: Int -> IO()
drawLow 0 = return ()
drawLow s1 = do
                putStr("^")
                drawLow (s1-1)

drawPien:: Int -> IO()
drawPien s1 = do
                putSpace (s1-1)
                putStr("||")
                putStrLn ("")
main :: IO ()
main = do 
         print "podaj n: "
         num <-  getLine
         let num2 = read num ::Int
         print num2
         drawTree num2 0
         drawLow (2*num2)
         putStrLn ("")
         drawPien num2

