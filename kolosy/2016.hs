--1
getSumNthNumb:: Integer -> Integer ->Integer 
getSumNthNumb 1 1 = 1
getSumNthNumb n k = getSumNthNumb (n `div` 10) k + (n `mod`10)^k

nPowers:: Integer -> [Integer]
nPowers 1 = [1]
nPowers n = loop 100 (n-1) 

loop:: Integer -> Integer ->[Integer]
loop 1 1 = [1]
loop n k = let x = getSumNthNumb n k
    in if x == n then x : loop (n-1) k else  loop (n-1) k
    

