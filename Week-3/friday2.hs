pairDifference :: (Int, Int) -> Int
pairDifference (x, y) = x - y

reversePair :: (a, b) -> (b, a)
reversePair (a, b) = (b, a)

sumPair :: (Int, Int) -> Int
sumPair (x, y) = x + y

makePair :: a -> b -> (a, b)
makePair x y = (x, y)

data Operation =
    Add Int Int
    | Subtract Int Int
    | Multiply Int Int
    deriving (Show)

evaluateOperation :: Operation -> Int
evaluateOperation (Add x y) = x + y
evaluateOperation (Subtract x y) = x - y
evaluateOperation (Multiply x y) = x * y

operationName :: Operation -> String
operationName (Add x y) = "addition"
operationName (Subtract x y) = "subtraction"
operationName (Multiply x y) = "multiplication"

swapOperation :: Operation -> Operation
swapOperation (Add x y) = Add y x
swapOperation (Subtract x y) = Subtract y x
swapOperation (Multiply x y) = Multiply y x

addOperands :: Operation -> Int
addOperands (Add x y) = x + y
addOperands (Subtract x y) = x + y
addOperands (Multiply x y) = x + y

totalOperands :: Operation -> Operation -> Int
totalOperands x y = addOperands x + addOperands y