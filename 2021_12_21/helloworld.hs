przywitajSie :: String -> String
przywitajSie imie = "Czesc " ++ imie ++". Witam serdecznie!"

main :: IO ()
main = do 
         print "Podaj swoje imie: "
         imie <- getLine
         print $ przywitajSie imie
         print $ "Twoje imie ma " ++ show (length imie) ++ " liter."
