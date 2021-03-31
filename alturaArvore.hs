data Tree a = Leaf a 
            | Branch (Tree a) (Tree a)
     
altura :: Tree a -> Int
altura Leaf a = 1
altura (Branch a esquerda direita) = if altura esquerda > altura direita then 
   (altura esquerda + 1)
   else 
   (altura direita + 1)
