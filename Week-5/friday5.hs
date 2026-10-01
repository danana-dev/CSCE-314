data Tree a =
    Leaf a
    | Node (Tree a) (Tree a)
    deriving (Show, Eq)

largeTree :: Tree Int
largeTree =
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

treeHeight :: Tree a -> Int
treeHeight (Leaf _) = 0
treeHeight (Node l r) = 1 + max (treeHeight l) (treeHeight r)

treeSize :: Tree a -> Int
treeSize (Leaf _) = 1
treeSize (Node l r) = treeSize l + treeSize r

treeSum :: Tree Int -> Int
treeSum (Leaf n) = n
treeSum (Node l r) = treeSum l + treeSum r

treeLeaves :: Tree a -> [a]
treeLeaves (Leaf n) = [n]
treeLeaves (Node l r) = treeLeaves l ++ treeLeaves r

listToTree :: [Int] -> Tree Int
listToTree [x] = Leaf x
listToTree xs = Node (listToTree (take (div (length xs) 2) xs)) (listToTree (drop (div (length xs) 2) xs))

treeToList :: Tree Int -> [Int]
treeToList (Leaf x) = [x]
treeToList (Node l r) = treeToList l ++ treeToList r