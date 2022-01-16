todoList :: [IO ()]

todoList = [putChar 'a',
            do putChar 'b'
               putChar 'c',
            do c <- getChar
               putChar c]

sequence2_        :: [IO a] -> a -> IO ()
sequence2_ [] _    =  return ()
sequence2_ (a:as) x =  do 
                    sequence2_ as x
                    a x