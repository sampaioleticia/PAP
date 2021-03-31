--inverso :: [a] → [a] - OK
inverso [] = []
inverso (x:xs) = inverso xs ++ [x]
