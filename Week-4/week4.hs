{- 
    building lists by ranges
    [1..10] creates a list from 1 to 10
    [2, 4..20] creates a list 2, 4 to 20 in increments of 2
    take 20 [1,3..] takes the first 20 terms of the list without the list going to infinity
-}

numbers :: [Int]
numbers = [1..10]

evens :: [Int]
evens = [2,4..20]

countByFives :: [Int]
countByFives = [5,10..50]

naturals :: [Int]
naturals = [1..]

evenStream :: [Int]
evenStream = [2,4..]

{-
take 20 (drop 20 evenStream)
takes out the first 20 terms of the list and
takes the next 20 terms without the list going to infinity
-}

-- list comprehension
-- list of x * x where (|) x is fed a range from 1 to 10 or
-- you can pass through [evenStream] to get an infinite list of squares
squares :: [Int]
squares = [x * x | x <- [1..10]]

-- list of x where x is fed a range from 1 to 30 and satisfies the condition: is a multiple of three
multiplesOfThree :: [Int]
multiplesOfThree = [x | x <- [1..30], mod x 3 == 0]

evenSquares :: [Int]
evenSquares = [x * x | x <- [1..20], even x]


-- helper functions
triple :: Int -> Int
triple x = x * 3

isLarge :: Int -> Bool
isLarge x = x > 20

-- higher order functions (map, filter, fold)
-- apply the triple function to all elements in the numbers list and return the result
tripledNumbers :: [Int]
tripledNumbers = map triple numbers

largeTriples :: [Int]
largeTriples = filter isLarge tripledNumbers

-- applies (+) function to replace the cons operator (:) of the list with accumulator at 0
totalLargeTriples :: Int
totalLargeTriples = foldr (+) 0 tripledNumbers

-- partial application (currying)
{-
    a = map (* 2)
    a is now a function that is waiting for one more input (a list) to evaluate
-}

-- range
rangeList :: [Int]
rangeList = [10,20..100]

-- list comprehension
compList :: [Int]
compList = [x | x <- [1..50], odd x]

-- use each of higher order functions on lists
addTen :: [Int] -> [Int]
addTen x = map (+10) x

moreThanTen :: [Int] -> [Int]
moreThanTen x = filter (>10) x

subtractAll :: [Int] -> Int
subtractAll x = foldr (-) 0 x