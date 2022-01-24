binsearch :: [Int] -> Int -> Int -> Int -> Int -- list, value, low, high, return int
binsearch xs value low high
   | high < low       = -1
   | center > value  = binsearch xs value low (mid-1)
   | center < value  = binsearch xs value (mid+1) high
   | otherwise        = xs!!mid
   where
   mid = low + ((high - low) `div` 2)
   center = (xs!!mid)*(xs!!mid)*(xs!!mid)


is3 :: Int -> Maybe Int
is3 value = if calculated == -1 then Nothing  else Just calculated
    where calculated = binsearch [1..value] value 0 value

c:: Maybe Int -> Int
c (Just value) = 2* value +1
c Nothing = 0