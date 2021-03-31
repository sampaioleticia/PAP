--mapear :: (a → b) → [a] → [b]
mapear f [] = []
mapear f (x:xs) = f x : mapear f xs
