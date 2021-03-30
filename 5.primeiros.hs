--primeiros :: Int → [a] → [a] 
primeiros 0 xs = []
primeiros n [] = []
primeiros n (x:xs) = x : primeiros (n - 1) xs
