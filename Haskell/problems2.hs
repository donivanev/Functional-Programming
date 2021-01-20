main :: IO()
main = do
    print (incrementAllBy [1, 2] 3)
    print (multiplyAllBy [1, 2] 3)

incrementAllBy :: [Int] -> Int -> [Int]
incrementAllBy [] n       = []
incrementAllBy (x : xs) n = (x + n) : incrementAllBy xs n

incrementAllBy' :: [Int] -> Int -> [Int]
incrementAllBy' xs n = if null xs then [] else ( (head xs) + n : ( incrementAllBy (tail xs) n) )

incrementAllBy'' :: [Int] -> Int -> [Int]
incrementAllBy'' xs n = [x + n | x <- xs] 

multiplyAllBy :: [Int] -> Int -> [Int]
multiplyAllBy [] n       = []
multiplyAllBy (x : xs) n = (x * n) : multiplyAllBy xs n

multiplyAllBy' :: [Int] -> Int -> [Int]
multiplyAllBy' xs n = [x * n | x <- xs] 

filterSmallerThan :: [Int] -> Int -> [Int]
filterSmallerThan xs n = [x | x <- xs, x > n]

isAscending :: Int -> Bool
isAscending n = helper (toList n)
    where
        toList n            = if n <= 0 then [] else mod n 10 : toList (div n 10)
        helper [x]          = True
        helper (x : y : xs) = if x < y then False else helper (y : xs)
    
isAscending' :: [Int] -> Bool
isAscending' xs
    |null xs || null (tail xs) = True
    |head xs >= head (tail xs) = False
    |otherwise                 = isAscending' (tail xs)

numToList :: Int -> [Int]
numToList n = if n < 10 then [n] else (mod n 10) : numToList (div n 10)