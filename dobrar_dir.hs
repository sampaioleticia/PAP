--dobrar_dir :: (a → b → b) → b → [a] → b
dobrar_dir f x [] = x
dobrar_dir f x (y:ys) = foldr f x (y:ys)
