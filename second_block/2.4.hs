isAttacking :: (Char, Int) -> (Char, Int) -> Bool
isAttacking (x1, y1) (x2, y2) =
    x1 == x2 || y1 == y2 || abs((fromEnum x1 - fromEnum 'a') - (fromEnum x2 - fromEnum 'a')) == abs (y1 - y2)

queensAttackEachOther :: [(Char, Int)] -> Bool
queensAttackEachOther [] = False
queensAttackEachOther [_] = False
queensAttackEachOther (p:ps) = any (isAttacking p) ps || queensAttackEachOther ps

test :: Int -> Bool
test ab = 
  let a = [('a', 1), ('b', 2), ('c', 4), ('d', 3)]
      b = [('a', 1), ('b', 3), ('c', 5), ('d', 7)]
      c = [('a', 1), ('b', 3), ('c', 5), ('h', 8)]
      d = [('a', 2), ('b', 4), ('c', 1), ('h', 8)]
  in
      (queensAttackEachOther a) == True &&
      (queensAttackEachOther b) == False &&
      (queensAttackEachOther c) == True &&
      (queensAttackEachOther d) == False