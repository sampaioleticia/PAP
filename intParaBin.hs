--intParaBin :: Int → String 
intParaBin 0 = "0"
intParaBin 1 = "1"
intParaBin n = intParaBin(n `div` 2) ++ show(n `mod` 2)
