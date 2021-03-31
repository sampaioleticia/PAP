data Tree a = Leaf a 
            | Branch (Tree a) (Tree a)
     
altura :: Tree a -> Int
altura (Leaf x) = 1
altura (Branch x esquerda direita) = max (altura esquerda) (altura direita) + 1
