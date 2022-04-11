main :: IO()
main = do
    --print ([5,10,15,[]])
    --print ([[],[[3,2,1]]])
    --print ([[1,2],3,[4,5,6]])
    print(reverseOrdSuff 37563) -- -> 36
    print(reverseOrdSuff 32763) -- -> 367
    print(reverseOrdSuff 32657) -- -> 7
    print(reverseOrdSuff 32666) -- -> 6
    print(sumUnique [[1, 2, 3, 2], [-4, -4], [5]]) -- -> 9
    --print([[2, 2, 2], [3, 3, 3], [4, 4, 4]]) -- -> 0
    --print([[1, 2, 3], [4, 5, 6], [7, 8, 9]]) -- -> 45
    print (averageOfProducts store2)
    print (closest store2 (averageOfProducts store2) (findMax store2 0))
    --print(findMax store1 0)
    print (closestToAverage store2) -- -> "cheese"
    --print (cheaper store2 "bread" 0)
    --print (cheaperAlternative store2) -- -> 1
    --print (sumUniqueHelper (unique [1, 2, 3, 2]))
    --print (sumUniqueHelper (unique [-4, -4]))
    --print (sumUniqueHelper (unique [5]))
    print (orderedTree tree)
    print (orderedTree tree2)
    print (minDistance points)

numToList :: Int -> [Int]
numToList 0 = []
numToList x = x `mod` 10 : numToList (x `div` 10)

reverseNum :: Int -> Int
reverseNum = read . reverse . show

listToNum :: [Int] -> Int
listToNum = read . concatMap show

reverseOrdSuffHelper :: [Int] -> [Int] -> [Int] 
reverseOrdSuffHelper numList xs
    |null numList                       = xs
    |length numList == 1                = xs ++ [head numList]
    |head numList > head (tail numList) = reverseOrdSuffHelper (tail numList) (xs ++ [head numList])
    |otherwise                          = reverseOrdSuffHelper (tail numList) []

reverseOrdSuff :: Int -> Int
reverseOrdSuff k = reverseNum (listToNum (reverseOrdSuffHelper (numToList (reverseNum k)) []))

{-uniqueElements :: Eq a => [a] -> [a] -> [a]
uniqueElements x []      = x 
uniqueElements [] (a:xs) = uniqueElements [a] xs
uniqueElements x (a:xs)  = if a `elem` x then uniqueElements x xs else uniqueElements (a:x) xs

uniqueElements' :: [Int] -> [Int]
uniqueElements' xs = [x | (x, y) <- zip xs [0..], x `notElem` (take y xs)]-}

unique :: [Int] -> [Int]
unique [] = []
unique (x : xs)
    |elem x (unique xs) = [ y | y <- (unique xs), y /= x ]
    |otherwise          = x : (unique xs)

sumUniqueHelper :: [Int] -> Int 
sumUniqueHelper [] = 0
sumUniqueHelper (x : xs) = x + sumUniqueHelper xs 

sumUnique :: [[Int]] -> Int
sumUnique (xs : listOfXS) = if null listOfXS then sumUniqueHelper (unique xs) else sumUniqueHelper (unique xs) + sumUnique listOfXS

type Product = (String, Double)
type StoreAvailability = [Product]

store1 = [("bread", 1), ("milk", 2.5), ("lamb", 10), ("cheese", 5), ("butter", 2.3)]
store2 = [("bread", 1), ("cheese", 2.5), ("bread", 1), ("cheese", 5), ("butter", 2.3)]

average :: [Product] -> Double
average [] = 0
average ((_, price) : xs) = price + average xs

averageOfProducts :: [Product] -> Double
averageOfProducts xs = average xs / fromIntegral (length xs)

findMax :: [Product] -> Double -> Double
findMax (x@(_, price) : xs) maxPrice
    |null xs = maxPrice
    |price > maxPrice = findMax xs price 
    |otherwise = findMax xs maxPrice

closest :: [Product] -> Double -> Double -> Double
closest (x@(_, price) : xs) avrg max
    |null xs = max
    |abs (avrg - price) < max = closest xs avrg price
    |otherwise = closest xs avrg max 

returnStrByValue :: [Product] -> Double -> Bool -> String
returnStrByValue (x@(name, price) : xs) value flag
    |null xs = ""
    |flag = returnStrByValue xs value True
    |price == value = name ++ returnStrByValue xs value True
    |otherwise = returnStrByValue xs value False

closestToAverage :: StoreAvailability -> String 
closestToAverage products = returnStrByValue products (closest products (averageOfProducts products) (findMax products 0)) False

--cheaper :: [Product] -> String -> Int -> Int
--cheaper (x@(name, price) : xs) product counter
--   |null xs = min
--    |name == product && price < min = cheaper xs name counter + 1 
--    |name /= product = cheaper xs name counter
--    |otherwise = cheaper xs product counter

{-
cheaperAlternative :: Shop -> Int
cheaperAlternative xs = length $ filter hasTwoPrices $ groupPrices xs
    where
        names = nub [name | (name, _, _) <- xs] -- nub е вградена ф-ия от библиотеката Data.List, която изтрива повторенията в списък. Тук ще получим списък от имената на продукти, без повторения

        groupPrices :: Shop -> [[Float]]
        groupPrices xs = [[price | (name', _, price) <- xs, name' == name] | name <- names] -- тук ще получим списък от списъци, всеки съдържащ различните цени на даден продукт 

        hasTwoPrices xs = length (nub xs) > 1 -- ако има две различни цени, то след като премахнем повторенията трябва списъкът да е с дължина поне 2
-}

points = [(1, 1, 2), (5.4, 3.3, 9.8), (10, 3, 6.6), (2.3, 5.5, 4.7)]

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z

distance :: (Double, Double, Double) -> (Double, Double, Double) -> Double
distance point1 point2 
    = (fst3 point1 - fst3 point2) * (fst3 point1 - fst3 point2) + (snd3 point1 - snd3 point2) * (snd3 point1 - snd3 point2) + (trd3 point1 - trd3 point2) * (trd3 point1 - trd3 point2)

minDistance :: [(Double,Double,Double)] -> Double
minDistance xs = minimum [distance p1 p2 | p1 <- xs, p2 <- xs, p1 /= p2] -- изграждаме списък от всички двойки различни точки, за всяка двойка изчисляваме съответното разстояние, и взимаме минималното
    --where distance (x1,y1,z1) (x2,y2,z2) = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2)

--minDist :: [(Double, Double, Double)] -> Double -> Double
--minDist [] min = 0
--minDist xs min = foldl (\ p1 p2 -> if distance p1 p2)

--minDistance :: [(Double,Double,Double)] -> Double
--minDistance xs = minimum (mapDistances xs)

{-reduceStr :: String -> String
reduceStr str = if str == reduced then str else reduceStr reduced
    where
        reduced = reduce str []
        reduce []  result = result
        reduce [c] result = result ++ [c]
        reduce (c1:c2:str) result
            |isLower c1 && isUpper c2 && c1 == toLower c2 = result ++ str
            |isUpper c1 && isLower c2 && c1 == toUpper c2 = result ++ str
            |otherwise                                    = reduce (c2:str) (result ++ [c1])

reduceStr' :: String -> String
reduceStr' str = if str == reduced then str else reduceStr' reduced
    where
        reduced = reduce str
        diff = ord 'a' - ord 'A'
        reduce = concat . (filter ((==1) . length)) . (groupBy (\ a b -> abs (ord a - ord b) == diff))-}
        -- groupBy работи подобно на group, но не сравнява елементите директно по (==), а по подаден от 
        -- нас предикат. В този случай смятаме две букви за равни, ако са на разстояние diff една от 
        -- друга - това е разстоянието между малка и главна буква в ASCII таблицата. Тоест след 
        -- групирането ще имаме списък от списъци, който съдържа двуелементни списъци от двойките 
        -- които искаме да унищожим, и едноелементни списъци от останалите букви. Чрез filter махаме 
        -- двойките, и чрез concat получаваме обикновен низ.

maximize :: (Ord a, Num a) => [a -> a] -> (a -> a)
maximize fs x = snd (maximum [(abs (f x), f x) | f <- fs])

inverseFun :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Bool
inverseFun f g a b = and [f (g x) == x && g (f x) == x | x <- [a..b]]

data BTree = NullT | Node (Float, Float) BTree BTree

tree = Node (3.0,10.0) (Node (5.0,8.0) (Node (6.0,7.0) NullT NullT)
                                       (Node (4.0,9.0) NullT NullT))
                       (Node (2.0,12.0) NullT
                                       (Node (1.0,15.0) NullT NullT))

tree2 = Node (3.0,10.0) (Node (5.0,8.0) (Node (6.0,7.0) NullT NullT)
                                        (Node (7.0,9.0) NullT NullT))
                        (Node (2.0,12.0) NullT
                                        (Node (1.0,15.0) NullT NullT))

subInterval :: (Float, Float) -> (Float, Float) -> Bool
subInterval (a1, b1) (a2, b2) = a2 <= a1 && b1 <= b2 -- проверка дали първият аргумент е подинтервал на втория

orderedTree :: BTree -> Bool
orderedTree NullT = True
orderedTree (Node _ NullT NullT) = True
orderedTree (Node n left@(Node leftInterval _ _) NullT) = leftInterval `subInterval` n && orderedTree left
orderedTree (Node n NullT right@(Node rightInterval _ _)) = n `subInterval` rightInterval && orderedTree right
orderedTree (Node n left@(Node leftInterval _ _) right@(Node rightInterval _ _)) 
    = leftInterval `subInterval` n && n `subInterval` rightInterval && orderedTree left && orderedTree right