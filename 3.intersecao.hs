--intersecao :: Eq a => [a] → [a] → [a] 
intersecao [] _ = []
intersecao (x:xs) l | elem x l = x : intersecao xs l
                    | otherwise = intersecao xs l
