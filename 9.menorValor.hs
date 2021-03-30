--menorValor :: Ord a => [a] → a 
menorValor [] = 0
menorValor [x] = x
menorValor (x:y:xs) |x > y = menorValor (y:xs)
                    |x < y = menorValor (x:xs)
                    |x == y = menorValor (x:xs)
