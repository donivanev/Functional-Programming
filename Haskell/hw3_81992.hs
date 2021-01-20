import Data.List(group)

main :: IO()
main = do
    print(encode "aaaaabbbb")
    print(encode "Haskell") -- -> "Haskell"
    print(encode "aaabccdefff") -- -> "3abccde3f"
    print(encode "aaaaaaaaaaaabbb") -- -> "12a3b"
    print(encode "aabbb") -- -> "aa3b"
    print(decode "aa3b")
    print(decode "12a3b") -- -> "aaaaaaaaaaaabbb"
    print(decode "a3b") -- -> "abbb"
    print(decode "aa3b") -- -> "aabbb"

repeatString :: Int -> Char -> String 
repeatString 0 _ = []
repeatString n c = c : repeatString (n - 1) c

encodeString :: String -> Int -> String -> String   
encodeString str counter xs
    |length str == 1 && length (show counter ++ [head str]) > counter = xs ++ repeatString (counter + 1) (head str)
    |length str == 1                                                    = xs ++ show (counter + 1) ++ str 
    |head str == head(tail str)                                         = encodeString (tail str) (counter + 1) xs
    |head str /= head(tail str) && length (show counter ++ [head str]) > counter 
                                                                        = encodeString (tail str) 0 (xs ++ repeatString (counter + 1) (head str))
    |otherwise                                                          = encodeString (tail str) 0 (xs ++ show (counter + 1) ++ [head str])

encode :: String -> String 
encode str = encodeString str 0 ""

isDigit :: Char -> Bool 
isDigit x = x >= '0' && x <= '9'

isAlpha :: Char -> Bool 
isAlpha x = (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') 

reverseNum :: Int -> Int
reverseNum = read . reverse . show

decodeString :: String -> Int -> Int -> String -> String
decodeString str num p xs
    |null str                      = xs
    |isDigit (head str)            = decodeString (tail str) (num + ((read [head str] :: Int) * 10 ^ p)) (p + 1) xs 
    |isAlpha (head str) && num < 1 = decodeString (tail str) 0 0 (xs ++ [head str])
    |otherwise                     = decodeString (tail str) 0 0 (xs ++ repeatString (reverseNum num) (head str)) 

decode :: String -> String 
decode str = decodeString str 0 0 ""

{-encode :: String -> String
encode str = helper str 1
    where 
        helper s counter
            |[]           = []
            |(x : y : xs) = if x == y then incrementValue counter else counter : x : helper (drop counter str) 1

decodeString :: String -> Int -> String -> String 
decodeString str counter
    |null str       = ""
    |x : str = if isDigit x then x : decodedString else decodedString : x : helper s (incrementValue power) decodedString-}

enc :: String -> String 
enc "" = []
enc xs = [(length x, head x) | x <- group xs]