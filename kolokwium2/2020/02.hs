usun2 :: String -> Char -> String
usun2 word char = filter(\x->x/= char) word

usun :: String -> String -> String
usun list args = foldl (\acc x -> usun2 acc x) list args