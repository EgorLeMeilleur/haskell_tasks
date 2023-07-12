gcd' :: Integer -> Integer -> Integer
gcd' n 0 = n
gcd' n m = gcd' m (n `mod` m)

scm :: Integer -> Integer -> Integer
scm n m 
    | n == 0 || m == 0 = error "Invalid arguments"
    | otherwise = n * m `div` gcd' n m