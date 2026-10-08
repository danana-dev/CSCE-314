data Expr =
    Value Int
    | Add Expr Expr
    | Subtract Expr Expr
    | Multiply Expr Expr
    | Divide Expr Expr
    deriving (Show, Eq)

-- skip any whitespaces and concatenate non-white space characters with the rest of the string
removingSpaces :: String -> String
removingSpaces [] = []
removingSpaces (x:xs)
    | x == ' ' = removingSpaces xs
    | otherwise = x : removingSpaces xs

-- parseExpression :: String -> Expr

-- parseAddSub :: String -> Expr


-- parseMulDiv :: String -> Expr

parseNumber :: String -> Expr
parseNumber x = Value (read x)

-- eval :: Expr -> Int

-- showExpr :: Expr -> String

-- calculate :: String -> Int