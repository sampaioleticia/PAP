--dobrar_esq :: (b → a → b) → b → [a] → b
dobrar_esq f x [] = x
dobrar_esq f x (y:ys) = foldl f x (y:ys)
