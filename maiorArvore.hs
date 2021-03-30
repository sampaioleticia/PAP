data Tree a = Leaf a 
            | Branch (Tree a) (Tree a)
maior :: Ord a => Tree a → a
maior (Branch x Leaf _ ) = x
maior (Branch x rightNode _ ) = maior rightNode _
