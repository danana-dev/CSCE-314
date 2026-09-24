-- zipWith: takes an operation, two lists,
-- and creates a new list with the size being the minimum size of the two
-- with elements being the result of the operation between elements sharing the same index  
sums :: [Int]
sums = zipWith (+) [1,2,3] [10,20,30]

products :: [Int]
products = zipWith (*) [1..] [2,4,6,8]

combineScore :: Int -> Int -> Int
combineScore exam bonus = exam + bonus

combineScores :: [Int]
combineScores = zipWith combineScore [70,82,91] [5, 10, 9]

largeOfTwo :: Int -> Int -> Int
largeOfTwo x y = max x y

bestValues :: [Int]
bestValues = zipWith largeOfTwo [70,80,90] [75,71,90]

-- binary tree
data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show) -- print out the tree

smallTree :: Tree Int
smallTree = Node (Leaf 3) (Leaf 4)

largeTree :: Tree Int
largeTree = Node ( Node (Leaf 2) (Leaf 5) ) ( Node (Leaf 7) (Leaf 1) )

tree8 :: Tree Int
tree8 =
    Node
        ( Node
            ( Node (Leaf 1) (Leaf 2) )
            ( Node (Leaf 3) (Leaf 4) ) )
        ( Node
            ( Node (Leaf 5) (Leaf 6) )
            ( Node (Leaf 7) (Leaf 8) ) )

{-
    tree16
    treeSize tree16
    treeHeight tree16
    sumTree tree16
-}
tree16 :: Tree Int
tree16 =
    Node
        ( Node 
            ( Node 
                ( Node (Leaf 1) (Leaf 2) ) 
                ( Node (Leaf 3) (Leaf 4) ) )
            ( Node
                ( Node (Leaf 5) (Leaf 6) )
                ( Node (Leaf 7) (Leaf 8) ) ) )
        ( Node 
            ( Node 
                ( Node (Leaf 9) (Leaf 10) ) 
                ( Node (Leaf 11) (Leaf 12) ) )
            ( Node
                ( Node (Leaf 13) (Leaf 14) )
                ( Node (Leaf 15) (Leaf 16) ) ) )


wordTree :: Tree String
wordTree = Node (Leaf "Left") (Leaf "Right")

-- structural recursion: count leaves
treeSize :: Tree a -> Int
treeSize (Leaf _) = 1
treeSize (Node l r) = treeSize l + treeSize r

treeHeight :: Tree a -> Int
treeHeight (Leaf _) = 0
treeHeight (Node l r) = 1 + max (treeHeight l) (treeHeight r)

sumTree :: Tree Int -> Int
sumTree (Leaf n) = n
sumTree (Node l r) = sumTree l + sumTree r