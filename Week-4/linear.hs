type Vector = [Int]

vectorAdd :: Vector -> Vector -> Vector
vectorAdd [] [] = []
vectorAdd (x:xs) (y:ys) = x + y : vectorAdd xs ys

vectorSubtract :: Vector -> Vector -> Vector
vectorSubtract [] [] = []
vectorSubtract (x:xs) (y:ys) = x - y : vectorSubtract xs ys

scalarMultiply :: Int -> Vector -> Vector
scalarMultiply s v = map (*s) v

dotProduct :: Vector -> Vector -> Int
dotProduct [] [] = 0
dotProduct (x:xs) (y:ys) = x * y + dotProduct xs ys

vectorSum :: Vector -> Int
vectorSum v = foldr (+) 0 v

countPositive :: Vector -> Int
countPositive v = length (filter (>0) v)

magnitudeSquared :: Vector -> Int
magnitudeSquared v = dotProduct v v

distanceSquared :: Vector -> Vector -> Int
distanceSquared v1 v2 = magnitudeSquared (vectorSubtract v1 v2)