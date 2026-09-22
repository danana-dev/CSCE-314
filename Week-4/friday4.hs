-- 1
countRange :: Int -> Int -> [Int]
countRange x y = [x..y]

-- 2
firstMultiples :: Int -> Int -> [Int]
firstMultiples x y = take y [x,x+x..]

-- 3
squaresThrough :: Int -> [Int]
squaresThrough x = take x [y * y | y <- [1..]]

-- 4
multiplesThrough :: Int -> Int -> [Int]
multiplesThrough x y = [z | z <- [x..y], mod z x == 0]

-- 5
scaleValues :: Int -> [Int] -> [Int]
scaleValues x xs = map (*x) xs

-- 6
keepAbove :: Int -> [Int] -> [Int]
keepAbove x xs = filter (>x) xs

-- 7
-- 1 : (2 : (3 : []))
-- 1 + 2 + 3 + 0
sumSquaresThrough :: Int -> Int
sumSquaresThrough x = foldl (+) 0 (squaresThrough x)

-- 8
sumScaledMultiples :: Int -> Int -> Int -> Int
sumScaledMultiples x y z = foldl (+) 0 (scaleValues y (multiplesThrough x z))