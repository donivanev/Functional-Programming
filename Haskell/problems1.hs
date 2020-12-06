main :: IO()
main = do
    print (mymin 16 11)
    print (16 `mymin` 11)
    print (isInside 6 1 10)
    print (myfunc 10 20)
    print (myfunc' 10 20)
    print (myFib 5)
    print (myFibIter 500)
    print (mygcd 5 15)

{- 
    Задача 1. Да се напише функция mymin, която приема два аргумента
    и връща по-малкия от тях.
-}

mymin :: Int -> Int -> Int
mymin a b = if a < b then a else b

{-
    Задача 2. Да се дефинира функцията isInside x a b, която проверява
    дали числото x се намира в затворения интервал [a, b].
-}

isInside :: Double -> Double -> Double -> Bool
isInside x a b = a <= x && x <= b

{-
    Задача 3. Да се напише функция myfunc, която пресмята средно
    аритметично на квадратите на 2 числа.
-}

myfunc :: Double -> Double -> Double
myfunc a b = (a * a + b * b) / 2

myfunc' :: Double -> Double -> Double
myfunc' a b = average (square a) (square b)
    where
        average a b = (a + b) / 2
        square x = x * x

{-
    Задача 4. Да се напише myfib, която получава един аргумент n и връща
    n-тото число на Фибоначи. Да се напише и итеративно решение.
    (Заб.: редицата е 1, 1, 2, 3, 5, 8, ... и е индексирана от 0.)
-}

myFib :: Int -> Int
myFib n
    | n == 0    = 1
    | n == 1    = 1
    | otherwise = myFib (n - 1) + myFib (n - 2)

myFib' :: Int -> Int
myFib' 0 = 1
myFib' 1 = 1
myFib' n = myFib' (n - 1) + myFib' (n - 2)

myFibIter :: Integer -> Integer
myFibIter n = helper 0 0 1
    where
        helper i next current
            | i > n     = next
            | otherwise = helper (i + 1) (current + next) next

{-
    Задача 5. Да се напише функция mygcd a b, която връща НОД(a, b).
-}

mygcd :: Int -> Int -> Int
mygcd a b
    | a == b    = a
    | a > b     = mygcd (a - b) b
    | otherwise = mygcd a (b - a)

{-
    Задача 6. Да се напише функция mymaxdivisor x, която намира най-големия
    делител d на цялото число x > 1, за който d < x.
-}

mymaxadvisor :: Int -> Int
mymaxadvisor x = helper (x - 1)
    where
        helper d
            | x `mod` d == 0 = d
            | otherwise      = helper (d - 1)

{-
    Задача 7. Да се дефинира функция, която намира сумата на нечетните числа
    в затворения интервал [a, b].
-}

sumOdd :: Int -> Int -> Int
sumOdd a b
    | a > b          = 0
    | a `mod` 2 == 1 = a + sumOdd (a + 1) b
    | otherwise      = sumOdd (a + 1) b