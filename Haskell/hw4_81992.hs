main :: IO()
main = do
    print (getAverageBalance (accounts1, people1) (\ (_, _, city) -> city == "Burgas")) -- -> 24.95 (24.950000000000003)
    print (getAverageBalance (accounts1, people1) (\ (_, (n:_), _) -> n == 'P')) -- -> 18.85
    print (averageBalanceOfCities (accounts1, people1) ["Sofia", "Plovdiv"])
    --print (averageBalanceOfCities (accounts1, people1) ["Sofia", "Gabrovo", "Stara Zagora"]) -- -> 67.85
    print (countInteresting t1)
    print (countInteresting t2)

type Account = (Int, Int, Double)
type Person = (Int, String, String)

people1 = [(1, "Ivan", "Sofia"), (2, "Georgi", "Burgas"), (3, "Petar", "Plovdiv"), (4, "Petya", "Burgas")]
accounts1 = [(1, 1, 12.5), (2, 1, 123.2), (3, 2, 13.0), (4, 2, 50.2), (5, 2, 17.2), (6, 3, 18.3), (7, 4, 19.4)]

--Non-optimal solution but works

filterPeople :: [Person] -> (Person -> Bool) -> [Person]
filterPeople xs p = [x | x <- xs, p x]

sumPersonBalances :: Person -> [Account] -> Double
sumPersonBalances p@(idP, _, _) ((_, idA, balance) : xs) 
    |null xs    = if idA == idP then balance else 0
    |idA == idP = balance + sumPersonBalances p xs 
    |otherwise  = 0 + sumPersonBalances p xs

countAccounts :: Person -> [Account] -> Double
countAccounts p@(idP, _, _) ((_, idA, _) : xs) 
    |null xs    = if idA == idP then 1 else 0
    |idA == idP = 1 + countAccounts p xs 
    |otherwise  = 0 + countAccounts p xs 

sumPeopleBalances :: [Person] -> [Account] -> Double
sumPeopleBalances (x : xs) ys = if null xs then sumPersonBalances x ys else sumPersonBalances x ys + sumPeopleBalances xs ys

countAllAccounts :: [Person] -> [Account] -> Double
countAllAccounts (x : xs) ys = if null xs then countAccounts x ys else countAccounts x ys + countAllAccounts xs ys

getAverageBalance :: ([Account], [Person]) -> (Person -> Bool) -> Double 
getAverageBalance pair p = sumPeopleBalances (filterPeople (snd pair) p) (fst pair) / countAllAccounts (filterPeople (snd pair) p) (fst pair)

getPairsCity :: [Person] -> [String] -> [Person]
getPairsCity xs str = [x | x <- xs, (\ (_, _, city) -> city `elem` str) x]
 
averageBalanceOfCities :: ([Account], [Person]) -> [String] -> Double 
averageBalanceOfCities pair str = sumPeopleBalances (getPairsCity (snd pair) str) (fst pair) / countAllAccounts (getPairsCity (snd pair) str) (fst pair) 

data BTree = Empty | Node Int BTree BTree

t1 :: BTree 
t1 =             Node 16 
    (Node 0 Empty Empty) (Node 4 
            (Node 1 Empty Empty) (Node 0 Empty Empty))

t2 :: BTree 
t2 =             Node 4 
    (Node 0 Empty Empty) (Node 2 
                (Node 1 Empty Empty) Empty) 

numberOfElements :: BTree -> Int
numberOfElements Empty               = 0
numberOfElements (Node _ left Empty) = 1 
numberOfElements (Node _ left right) = 2

countHeirs :: BTree -> Int -> Bool -> Int
countHeirs Empty _ _                = 0
countHeirs (Node _ Empty Empty) _ _ = 0
countHeirs (Node n left right) k p
    |p         = 0 + countHeirs left k True + countHeirs right k True
    |n == k    = numberOfElements (Node n left right) + countHeirs left k True + countHeirs right k True
    |otherwise = 0 + countHeirs left k False + countHeirs right k False

countInteresting :: BTree -> Int
countInteresting Empty = 0
countInteresting (Node n left right) 
    = if (2 ^ countHeirs (Node n left right) n False) == n then 1 + countInteresting left + countInteresting right else 0 + countInteresting left + countInteresting right