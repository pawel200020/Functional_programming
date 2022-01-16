sumsq :: Int -> Int
sumsq 0 = 0
sumsq n = sumsq (n-1)+n*n


main :: IO ()
main = do 
         print "podaj n: "
         nStr <- getLine
         let n = read nStr :: Int
         print (sumsq n)

