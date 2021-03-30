--ordenar :: Ord a => [a] → [a]
menorValor [] = 0
menorValor [x] = x
menorValor (x:y:xs) |x > y = menorValor (y:xs)
                    |x < y = menorValor (x:xs)
                    |x == y = menorValor (x:xs) 

remove_menorValor [] = []
remove_menorValor (x:xs) |(x == (menorValor (x:xs))) = xs
                         |otherwise = (x:remove_menorValor xs)

aux_ord listaord [] = listaord
aux_ord listaord (x:xs) = aux_ord (listaord ++ [menorValor (x:xs)]) (remove_menorValor (x:xs))

ordenar [] = []
ordenar listaordenada = aux_ord [] listaordenada
