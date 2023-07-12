solveHanoiTower :: Int -> [(Int, Int)]
solveHanoiTower n = hanoi n 1 2 3

hanoi :: Int -> Int -> Int -> Int -> [(Int, Int)]
hanoi 0 _ _ _ = []
hanoi n from to buf = hanoi (n-1) from buf to ++ [(from,to)] ++ hanoi (n-1) buf to from