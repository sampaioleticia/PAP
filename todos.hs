--todos :: [Bool] → Bool
todos [] = True
todos (True:xs) = todos xs
todos _ = False
