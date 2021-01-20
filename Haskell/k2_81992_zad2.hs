main :: IO()
main = do
    print (dominates (+4) (*2) [1..4]) -- -> True
    print (dominates (+4) (*2) [1..5]) -- -> False

--5 6 7 8 <-> 2 4 6 8
--5 6 7 8 9 <-> 2 4 6 8 10

compareLists :: [Int] -> [Int] -> Bool
compareLists xs ys
    |null xs           = True
    |head xs < head ys = False
    |otherwise         = compareLists (tail xs) (tail ys)

dominates :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
dominates f g xs = compareLists (map f xs) (map g xs)