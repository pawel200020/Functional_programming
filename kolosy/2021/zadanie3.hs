data Wr a = Wr [[a]]

dg :: Wr a -> [a] -> Wr a
dg (Wr a) array = (Wr (array:a))

ug :: Wr a -> Wr a
ug (Wr (a:as)) = (Wr(as))

de :: Wr a -> a -> Wr a
de (Wr (a:as)) elem = Wr ((elem:a):as)

ue :: Wr a -> Wr a
ue (Wr ((elem:a):as))= Wr (a:as)

lg :: Wr a -> Integer
lg (Wr (a)) = toInteger (length  a)

wr2l :: Wr a -> [a]
wr2l (Wr (a)) = concatMap (\x -> x) a