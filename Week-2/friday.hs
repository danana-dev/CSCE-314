-- takes one integer value and return value raised to the third power
cube :: Int -> Int
cube x = x * x * x

-- takes three integer values and returns the added result of all three
addThree :: Int -> Int -> Int ->Int
addThree x y z = x +  y + z

-- takes a character and string and returns the string with the character on both sides of the string 
surround :: Char -> String -> String
surround c s = c : s ++ [c]

-- takes a function and a value and applies the function to the value three times
applyThreeTimes :: (a -> a) -> a -> a
applyThreeTimes f x = f (f (f x))

-- takes two functions and an integer value and adds the result of the two functions on the integer
combineResults :: (Int -> Int) -> (Int -> Int) -> Int -> Int
combineResults f g x = f x + g x

-- takes an integer and returns the result of recursively adding the numbers up to the integer 
sumTo :: Int -> Int
sumTo 0 = 0
sumTo x = x + sumTo (x - 1)

-- takes a non-negative integer and returns a list containing numbers from that value down to 1
countDown :: Int -> [Int]
countDown 0 = []
countDown x = x : countDown (x - 1)

-- takes a string and returns a new string where each character appears twice // c : c : duplicateChars s works as well since construct operator works right-associated
duplicateChars :: String -> String
duplicateChars "" = []
duplicateChars (c:s) = [c] ++ [c] ++ duplicateChars s