main :: IO()
main = do
    print (chunksOf 2 [1, 3, 4, 6, 7, 8])
    print (divisors 21)
    print (isSorted [4, 8, 14])
    print (merge [1, 3, 7] [6, 9, 45])
    print (insert 5 [1, 4, 9])
    print (insertionSort' [7, 8, 3, 2, 0])

isImage :: [Int] -> [Int] -> Bool
isImage [_] [_]               = True
isImage (a1:a2:as) (b1:b2:bs) = a1 - b1 == a2 - b2 && isImage (a2:as) (b2:bs)

isImage' (a1 : as) (b1 : bs) = and (map (\ (a, b) -> a - b == a1 - b1) (zip as bs))

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

isTriangular :: [[Int]] -> Bool
isTriangular []    = False 
isTriangular [[_]] = True
isTriangular mat   = all (== 0) (tail (map head mat)) && isTriangular (tail (map tail mat))

divisors :: Int -> [Int]
divisors n = [d | d <- [1..(n-1)], mod n d == 0]

isPrime :: Integer -> Bool 
isPrime n = [1, n] == [d | d <- [1..(n-1)], mod n d == 0]

isPrime' :: Integer -> Bool 
isPrime' x = helper 1 0
    where
        helper n i
            |n < 2         = False   
            |mod n i == 0  = True
            |otherwise     = helper n (i + 1)

primesInRange :: Integer -> Integer -> [Integer]
primesInRange a b = [x | x <- [a..b], isPrime x]

primesInRange' :: Integer -> Integer -> [Integer]
primesInRange' a b = [x | x <- [a..b], prime x 2]
    where 
        prime x i
            |i >= x       = True
            |mod x i == 0 = False
            |otherwise    = prime x (i + 1)

divideBy3And7 :: Integer -> [Integer]
divideBy3And7 n = [n | n <- [1..n], mod n 3 == 0 && mod n 7 == 0]

prodSumDiv :: [Int] -> Int -> Int
prodSumDiv xs k = product [x | x <- xs , mod (sum (divisors x)) k == 0]

isSorted :: [Int] -> Bool 
isSorted [] = True 
isSorted [x] = True 
isSorted [x1, x2] = x1 <= x2 
isSorted (x1 : x2 : xs) = x1 <= x2 && isSorted (x2 : xs)

isSorted' xs = all (\ (a, b) -> a <= b) (zip xs (tail xs))

merge :: [Int] -> [Int] -> [Int] 
merge as [] = as 
merge [] bs = bs
merge (a : as) (b : bs) = if a <= b then a : merge as (b : bs) else b : merge (a : as) bs

insert :: Int -> [Int] -> [Int] 
insert num [] = [num] 
insert num xs@(c:cs) = if c > num then num : xs else c : insert num cs

insertionSort :: [Int] -> [Int] 
insertionSort cs = helper [] cs 
    where 
        helper result [] = result 
        helper result (c:cs) = helper (insert c result) cs

insertionSort' :: [Int] -> [Int]
insertionSort' xs = foldr insert [] xs

insertionSort'' :: [Int] -> [Int]
insertionSort'' = foldr insert []