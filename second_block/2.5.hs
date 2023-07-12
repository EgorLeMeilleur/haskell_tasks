isSelfDual :: [Int] -> Bool
isSelfDual vector = all check [i | i <- [0..n - 1]]
    where n = length vector `div` 2
          check i = vector !! i /= (vector !! (n * 2 - i - 1))

test :: Int -> Bool
test ab = 
  let a = [0, 1, 0, 1, 1, 0, 1, 0]
      b = [0, 1, 0, 1, 0, 1, 0, 1]
      c = [0, 1, 1, 0]
      d = [0, 1, 1, 1]
      e = [0, 0, 0, 0, 1, 1, 1, 1]
  in
      (isSelfDual a) == False &&
      (isSelfDual b) == True &&
      (isSelfDual c) == False &&
      (isSelfDual d) == False &&
      (isSelfDual e) == True