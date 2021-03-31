data Tree a = Leaf a 
            | Branch (Tree a) (Tree a)
            
menorValor [] = 0
menorValor [x] = x
menorValor (x:y:xs) |x < y = menorValor (y:xs)
                    |x > y = menorValor (x:xs)
                    |x == y = menorValor (x:xs)
                                 
pegarValor Leaf a = a
pegarValor (Branch a esquerda direita) = pegarValor esquerda ++ a ++ pegarValor direita

maior :: Ord a => Tree a -> a
maior a = menorValor (pegarValor a)
