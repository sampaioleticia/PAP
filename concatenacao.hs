--concatenacao :: [a] → [a] → [a] 
concatenacao xs ys = foldr (:) ys xs
