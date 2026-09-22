-- tuples
student :: (String, Int)
student = ("Danny", 100)

firstOfPair :: (a, b) -> a
firstOfPair (x, _) = x
secondOfPair :: (a, b) -> b
secondOfPair (_, x) = x
swapPair :: (a, b) -> (b, a)
swapPair (a, b) = (b, a)

-- lists
firstItem :: [a] -> a
-- extra case: firstItem [] = error "Empty List"
firstItem (x:_) = x

restOfList :: [a] -> [a]
restOfList (_:xs) = xs

copyList :: [a] -> [a]
copyList [] = []
copyList (x:xs) = x : copyList xs

-- datatypes
data TrafficLight
    = Red
    | Yellow -- (|) guard; else if
    | Green
    deriving (Show, Eq) -- allow TrafficLight to be printed and compared with equal/notequal

trafficAction :: TrafficLight -> String
trafficAction Red = "Stop"
trafficAction Yellow = "Caution"
trafficAction Green = "Go"

data Shape
    = Circle Double -- constructor arguments
    | Rectangle Double Double
    | Square Double
    deriving (Show)

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
area (Square l) = l * l

-- remaking a list using datatypes
data IntList
    = Empty
    | Cons Int IntList -- IntList at end to point towards another element
    deriving (Show)

exampleList :: IntList
exampleList = Cons 4 (Cons 7 (Cons 2 Empty))

sumIntList :: IntList -> Int
sumIntList Empty = 0
sumIntList (Cons x xs) = x + sumIntList xs -- since xs is defined as a IntList in the datatype, it can be passed through by itself

lengthIntList :: IntList -> Int
lengthIntList Empty = 0
lengthIntList (Cons _ xs) = 1 + lengthIntList xs

myExampleList2 :: IntList
myExampleList2 = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Empty))))