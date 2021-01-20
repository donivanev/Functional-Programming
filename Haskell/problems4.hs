main :: IO()
main = do
    print ""
    print (Rectangle 10 15)

data Shape = Circle Double |
             Rectangle Double Double |
             Triangle Double Double Double 
              deriving Show

--instance Show Shape where 
--  show (Triangle a b c) = show (a, b, c)
 --   show (Rectangle a b) = "Rectangle with sides" ++ show a ++ " i " ++ show b

a :: [Shape]
a = Circle 10 : [Rectangle 10 21]

perimeter :: Shape -> Double
perimeter (Circle r) = 3.14 * r * 2
perimeter (Rectangle a b) = 2 * (a + b)
perimeter (Triangle a b c) = a + b + c

area :: Shape -> Double 
area (Circle r)       = pi * r * r 
area (Rectangle a b)  = a * b 
area (Triangle a b c) = sqrt (p * (p - a) * (p - b) * (p - c)) 
    where p = (a + b + c) / 2

isRound :: Shape -> Bool 
isRound (Circle _) = True 
isRound _          = False

sumArea :: [Shape] -> Double 
sumArea shapes = sum (map area shapes)

biggestShape :: [Shape] -> Shape
biggestShape []     = error "Empty list"
biggestShape shapes = foldl1 (\ u v -> if area u > area v then u else v) shapes

data Point = Point2D Double Double | Point3D Double Double Double
    deriving (Eq, Show)

distance :: Point -> Point -> Double 
distance (Point2D x1 y1) (Point2D x2 y2)       = sqrt ((x1-x2) * (x1-x2) + (y1 - y2) * (y1 - y2)) 
distance (Point3D x1 y1 z1) (Point3D x2 y2 z2) = sqrt ((x1-x2) * (x1-x2) + (y1 - y2) * (y1 - y2) + (z1 - z2) * (z1 - z2))
distance _ _ = error "Not compatible"