--primeiros :: [(a, b)] → [a] 
primeiros [] = []
primeiros (x:xs) = fst x : primeiros xs
