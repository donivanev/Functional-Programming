main :: IO()
main = do
    print (isBinarySearchTree t1) -- -> True
    print (isBinarySearchTree t2) -- -> False (в дясното поддърво има стойности, помалки от тази в корена)
    print (isBinarySearchTree t3) -- -> False (лявото поддърво не е двоично дърво за търсене)

data BTree = Empty | Node Int BTree BTree

t1 :: BTree 
t1 = Node 8 (Node 3 (Node 1 Empty Empty) 
                    (Node 4 Empty Empty)) 
            (Node 10 (Node 9 Empty Empty) 
                    (Node 14 Empty Empty)) 

t2 :: BTree 
t2 = Node 8 (Node 3 (Node 1 Empty Empty) 
                    (Node 4 Empty Empty))
            (Node 8 (Node 5 Empty Empty) 
                    (Node 14 Empty Empty)) 

t3 :: BTree 
t3 = Node 8 (Node 3 (Node 5 Empty Empty) 
                    (Node 6 Empty Empty)) 
            (Node 10 (Node 9 Empty Empty) 
                    (Node 14 Empty Empty))

binarySearchTree :: BTree -> [Int]
binarySearchTree Empty                = []
binarySearchTree (Node n Empty Empty) = [n]
binarySearchTree (Node n left right)  = binarySearchTree left ++ [n] ++ binarySearchTree right

isSorted :: [Int] -> Bool
isSorted xs
    |null xs || length xs == 1 = True
    |head xs > head (tail xs)  = False
    |otherwise                 = isSorted (tail xs)

isBinarySearchTree :: BTree -> Bool
isBinarySearchTree bt = isSorted (binarySearchTree bt)