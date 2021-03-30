data Tree a = Leaf a 
            | Branch (Tree a) (Tree a)
     
altura :: Tree a -> Int
altura Leaf = 0
altura (Branch _ a b) = 1 + max (altura a) (altura b)
