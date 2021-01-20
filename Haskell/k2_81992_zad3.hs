main :: IO()
main = do
    print (splitPoints (1,1) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)]) -- -> ([(1.0,2.0),(2.0,3.0),(-1.0,1.0)],[(10.0,15.0),(12.0,14.0)]))

type Point = (Double, Double)

distance :: (Double, Double) -> Point -> Double
distance x y = sqrt ((fst x - fst y) ^ 2 + (snd x - snd y) ^ 2)

inCircle :: Point -> Double -> [Point] -> [Point]
inCircle p r ps = [point | point <- ps, (distance point p) < r]

outOfCircle :: Point -> Double -> [Point] -> [Point]
outOfCircle p r ps = [point | point <- ps, (distance point p) > r]

splitPoints :: Point -> Double -> [Point] -> ([Point], [Point])
splitPoints p r ps = (inCircle p r ps, outOfCircle p r ps)  
