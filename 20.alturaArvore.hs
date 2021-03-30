data Tree a = Leaf a 
            | Branch (Tree a) (Tree a)
altura :: Tree a → Int
altura Leaf _ = 0
altura (Branch leftNode _ rightNode _ ) = 1 + max (altura leftNode _ ) (altura rightNode _ )
