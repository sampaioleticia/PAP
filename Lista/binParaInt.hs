--binParaInt :: String → Int 
binParaInt 0 = 0
binParaInt n = 2 * binParaInt (div n 10) + (mod n 10)
