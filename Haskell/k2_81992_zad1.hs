main :: IO()
main = do
    print (findNb 1071225)         -- -> 45
    print (findNb 40539911473216)  -- -> 3568
    print (findNb 135440716410000) -- -> 4824
    print (findNb 4183059834009)   -- -> 2022
    print (findNb 91716553919377)  -- -> -1
    print (findNb 24723578342962)  -- -> -1

find :: Integer -> Integer -> Integer -> Integer
find n m sum 
    |sum > m   = -1
    |sum == m  = n - 1
    |otherwise = find (n + 1) m (sum + (n ^ 3)) 

findNb :: Integer -> Integer
findNb m = find 1 m 0 