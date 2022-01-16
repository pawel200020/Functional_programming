displayList :: [Int] -> IO ()
displayList[] = return ()
displayList (l:list) = do
                    print (l*l)
                    displayList list

main:: IO()
main = do
        print "podaj n: "
        nStr <- getLine
        let n = read nStr :: Int
        displayList [1..n]