import Data.Char (isDigit)

data ExprTree = 
    None
  | Tree String ExprTree ExprTree
instance Show ExprTree where
  show None = ""
  show (Tree x None None) = show x
  show (Tree x None right) = show x ++ "(" ++ show right ++ ")"
  show (Tree x left None) = "(" ++ show left ++ ")" ++ show x
  show (Tree x left right) = "(" ++ show left ++ ")" ++ show x ++ "(" ++ show right ++ ")"
endTreeTraversal :: ExprTree -> String
endTreeTraversal None = ""
endTreeTraversal (Tree x None None) = x
endTreeTraversal (Tree x left None) = endTreeTraversal left ++ " " ++ x
endTreeTraversal (Tree x None right) = endTreeTraversal right ++ " " ++ x
endTreeTraversal (Tree x left right) = endTreeTraversal left ++ " " ++ endTreeTraversal right ++ " " ++ x
createExprTree :: String -> ExprTree
createExprTree expr = head(foldl foldingFunction [] (words expr)) 
  where
  foldingFunction (x:y:ys) "*" = (Tree "*" y x):ys
  foldingFunction (x:y:ys) "+" = (Tree "+" y x):ys
  foldingFunction (x:y:ys) "-" = (Tree "-" y x):ys
  foldingFunction (x:y:ys) "/" = (Tree "/" y x):ys
  foldingFunction xs numberString = (Tree numberString None None):xs

operatorPriority :: String -> Int
operatorPriority "+" = 1
operatorPriority "-" = 1
operatorPriority "*" = 2
operatorPriority "/" = 2

makeRPNfromExpr :: String -> String
makeRPNfromExpr expr = make (words expr) [] []
  where 
    make [] [] out = out
    make [] (x:xs) out = make [] xs (out ++ x ++ " ") 
    make (x:xs) stack out 
      | isDigitmy x = make xs stack (out ++ x ++ " ")
      | otherwise = let (higherOps, lowerOps) = span (\op -> operatorPriority op >= operatorPriority x) stack
                    in make xs (x:lowerOps) (out ++ concatMap add_space higherOps)
        where add_space x = x ++ " "
              isDigitmy x = all isDigit x

solveRPN :: String -> Int
solveRPN = head . foldl foldingFunction [] . words
  where
  foldingFunction (x:y:ys) "*" = (x * y):ys
  foldingFunction (x:y:ys) "+" = (x + y):ys
  foldingFunction (x:y:ys) "-" = (y - x):ys
  foldingFunction (x:y:ys) "/" = (y `div` x):ys
  foldingFunction xs numberString = read numberString:xs

makeTree :: String -> ExprTree
makeTree a = createExprTree (makeRPNfromExpr a)
  
countexpr :: String -> Int
countexpr a = solveRPN (endTreeTraversal (createExprTree (makeRPNfromExpr a)))

test :: Int -> Bool
test ab = 
  let a = "2 + 3 * 6 - 1"
      b = "22 * 2 - 45 / 2"
      c = "45 - 41 + 2 * 2"
      d = "5 / 2 * 2 - 3 + 4 * 2 / 3"
      e = "22 * 3 - 23 * 2 - 1"
  in
      (countexpr a) == 19 &&
      (countexpr b) == 22 &&
      (countexpr c) == 8 &&
      (countexpr d) == 3 &&
      (countexpr e) == 19