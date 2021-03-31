--filtrar :: (a → Bool) → [a] → [a]
filtrar f [] = []
filtrar f (x:xs) | f x = x : filtrar f xs
                 | otherwise = filtrar f xs
