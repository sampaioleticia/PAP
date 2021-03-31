data Tree a = Leaf a 
            | Branch (Tree a) (Tree a)
            
menorValor [] = 0
menorValor [x] = x
menorValor (x:y:xs) |x < y = menorValor (y:xs)
                    |x > y = menorValor (x:xs)
                    |x == y = menorValor (x:xs)

maior :: Ord a => Tree a -> a
maior a = menorValor (esquerda ++ a ++ direita)
