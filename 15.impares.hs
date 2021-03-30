--impares :: [Int] → [Int]
filtrar f [] = []
filtrar f (x:xs) | f x = x : filtrar f xs
                 | otherwise = filtrar f xs
                
impares = filtrar odd
