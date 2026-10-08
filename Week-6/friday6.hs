-- CSCE 314 - Friday 6
-- TextTree

data TextTree
    = Word String
    | Join TextTree TextTree
    deriving (Show, Eq)


-- ------------------------------------------------------------
-- Sample Trees
-- ------------------------------------------------------------

smallTree :: TextTree
smallTree =
    Join
        (Word "functional")
        (Join
            (Word "programming")
            (Word "matters"))

messageTree :: TextTree
messageTree =
    Join
        (Join
            (Join
                (Word "functional")
                (Word "programming"))
            (Join
                (Word "uses")
                (Word "small")))
        (Join
            (Join
                (Word "composable")
                (Word "functions"))
            (Join
                (Word "to")
                (Join
                    (Word "model")
                    (Word "structure"))))


-- ------------------------------------------------------------
-- 1. wordCount
-- ------------------------------------------------------------

wordCount :: TextTree -> Int
wordCount (Word _) = 1
wordCount (Join l r) = wordCount l + wordCount r

-- ------------------------------------------------------------
-- 2. treeWords
-- ------------------------------------------------------------

treeWords :: TextTree -> [String]
treeWords (Word w) = [w]
treeWords (Join l r) = treeWords l ++ treeWords r


-- ------------------------------------------------------------
-- 3. renderText
-- ------------------------------------------------------------

renderText :: TextTree -> String
renderText t = unwords (treeWords t)

-- ------------------------------------------------------------
-- 4. totalCharacters
-- ------------------------------------------------------------

totalCharacters :: TextTree -> Int
totalCharacters (Word w) = length w
totalCharacters (Join l r) = totalCharacters l + totalCharacters r

-- ------------------------------------------------------------
-- 5. longestWord
-- ------------------------------------------------------------

longestWord :: TextTree -> String
longestWord (Word w) = w
longestWord (Join l r) = 
    let left = longestWord l
        right = longestWord r
    in if length left >= length right
       then left
       else right

-- ------------------------------------------------------------
-- 6. mapWords
-- ------------------------------------------------------------

mapWords :: (String -> String) -> TextTree -> TextTree
mapWords f (Word w) = Word (f w)
mapWords f (Join l r) = Join (mapWords f l) (mapWords f r)


-- ------------------------------------------------------------
-- 7. treeSummary
-- ------------------------------------------------------------

treeSummary :: TextTree -> (Int, Int)
treeSummary t = (wordCount t, totalCharacters t)


-- ------------------------------------------------------------
-- Challenge: foldTextTree
-- ------------------------------------------------------------

foldTextTree
    :: (String -> a)
    -> (a -> a -> a)
    -> TextTree
    -> a

foldTextTree wordFn joinFn (Word w) = wordFn w
foldTextTree wordFn joinFn (Join l r) = joinFn (foldTextTree wordFn joinFn l) (foldTextTree wordFn joinFn r)


-- ------------------------------------------------------------
-- Challenge: wordCountFold
-- ------------------------------------------------------------

wordCountFold :: TextTree -> Int
wordCountFold t = foldTextTree (const 1) (+) t
