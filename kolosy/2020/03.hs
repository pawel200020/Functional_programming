hasNumber:: Integer ->[Integer]->Bool 
hasNumber _ [] =False
hasNumber number list = if length (filter(==number) list) > 0 then True else False

hasAllLoverVals:: [Integer]->Integer->Bool
hasAllLoverVals array val=  if length (filter (==False) (map(\x->hasNumber x array) [1..(val-1)])) > 0 then False else True

isClosing:: Integer -> [Integer]->Bool 
isClosing index array |toInteger index +1 < array!! fromIntegral  index  = False 
                      |otherwise = hasAllLoverVals (take (fromIntegral  index) array) (array!! fromIntegral index)

closingIndexes:: [Integer]->[Integer]
closingIndexes array = filter (\x -> isClosing (x-1) array) [1..(toInteger(length array) )]
