--removerPrimeiro :: Eq a => [a] → a → [a]
removerPrimeiro [] n = []
removerPrimeiro (x:xs) n | x == n = xs
                         | otherwise = x : removerPrimeiro xs n
