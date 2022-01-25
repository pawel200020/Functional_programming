f5 :: (Foldable t, Num a) => ((a, b) -> a) -> t b -> a
--f5 f l= foldr (\x y -> f (y,x)) 0 l
--f5 f = foldr (\x y -> f (y,x)) 0
--f5 f = foldr (\x y -> (curry f) y x ) 0
--f5 f = foldr (\x y -> (flip(curry f)) x y) 0
--f5 f = foldr ((flip.curry) f) 0
f5 f = foldr ((flip.curry) f) 0
