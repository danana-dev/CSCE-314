-- basic tree stuff
data Tree a =
    Leaf a
    | Node (Tree a) (Tree a)
    deriving (Show, Eq)

tree1 :: Tree Int
tree1 =
    Node
        (Node (Leaf 1) (Leaf 2))
        (Node (Leaf 3) (Leaf 4))

tree2 :: Tree Int
tree2 =
    Node
        ( Node 
            ( Node 
                ( Node 
                    ( Node 
                        ( Leaf 2 )
                        ( Leaf 5 ))
                    ( Node 
                        ( Leaf 8 )
                        ( Leaf 11 )))
                ( Node 
                    (Leaf 14 )
                    (Leaf 17 )))
            ( Node 
                ( Leaf 20 )
                ( Leaf 23 )))
        ( Node 
            ( Leaf 26 )
            ( Node 
                ( Node 
                    ( Node  
                        ( Leaf 29 )
                        ( Leaf 32 ))
                    ( Leaf 35 ))
                ( Node 
                    ( Node 
                        ( Leaf 38 )
                        ( Leaf 41 ))
                    ( Node 
                        ( Leaf 44 )
                        ( Leaf 47 )))))

-- mapping over a tree
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)

doubleValue :: Int -> Int
doubleValue x = x * 2

squaredTree :: Tree Int
squaredTree = mapTree (\x -> x * x) tree1 -- lambda function but (^ 2) also works as the function input

doubledTree :: Tree Int
doubledTree = mapTree doubleValue tree2

-- folding a tree
foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree leafFn nodeFn (Leaf x) = leafFn x
foldTree leafFn nodeFn (Node left right) = nodeFn (foldTree leafFn nodeFn left) (foldTree leafFn nodeFn right) 

-- functions with foldtree
treeSize :: Tree a -> Int
treeSize = foldTree (const 1) (+) -- const is a function that returns the first value given to it (1)

treeSum :: Tree Int -> Int
treeSum = foldTree id (+) -- id is a function that returns the value given to it (the leaf value)

treeLeaves :: Tree a -> [a]
treeLeaves = foldTree (\x -> [x]) (++)

treeMaximum :: Tree Int -> Int
treeMaximum = foldTree id max

treeHeight :: Tree a -> Int
treeHeight = foldTree (const 0) (\leftHeight rightHeight -> 1 + max leftHeight rightHeight)

-- applying a quadratic function to a tree
quadratic :: Int -> Int
quadratic x = x^2 + 3*x + 4

quadraticTree :: Tree Int
quadraticTree = mapTree quadratic tree1

quadraticLambdaTree :: Tree Int
quadraticLambdaTree = mapTree (\x -> x^2 + 3*x + 4) tree1

{-
create your own equation function
map that equation to a tree
make the equation function a lambda tree
-}

timesTenPlusOne :: Int -> Int
timesTenPlusOne x = x * 10 + 1

timesTenPlusOneTree :: Tree Int
timesTenPlusOneTree = mapTree timesTenPlusOne tree1

timesTenPlusOneLambdaTree :: Tree Int
timesTenPlusOneLambdaTree = mapTree (\x -> x * 10 + 1) tree1