findAllDescendants :: String -> [(String, String)] -> [String]
findAllDescendants person familyTree = 
    let children = [child | (parent, child) <- familyTree, parent == person]
    in children ++ concatMap (`findAllDescendants` familyTree) children

test :: Int -> Bool
test ab = 
  let a = [("Ivan", "Gleb"), ("Gleb", "David"), ("David", "Viktor"), ("Viktor", "Egor")]
      b = [("Ivan", "Gleb"), ("Ivan", "David"), ("Ivan", "Viktor"), ("Mikhail", "Egor")]
  in
      (findAllDescendants "Ivan" a) == ["Gleb", "David", "Viktor", "Egor"] &&
      (findAllDescendants "ret" a) == [] &&
      (findAllDescendants "Ivan" b) == ["Gleb", "David", "Viktor"] &&
      (findAllDescendants "Mikhail" b) == ["Egor"]