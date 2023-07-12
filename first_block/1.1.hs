combinationsWithoutRepetitions :: Integer -> Integer -> Integer
combinationsWithoutRepetitions n m
    | n < 0 || m < 0 = error "Invalid arguments"
    | m > n = 0
    | m == 0 || n == 0 = 1
    | otherwise = combinationsWithoutRepetitions (n - 1) m + combinationsWithoutRepetitions (n - 1) (m - 1)

combinationsWithRepetitions :: Integer -> Integer -> Integer
combinationsWithRepetitions n m = combinationsWithoutRepetitions (n + m - 1) m