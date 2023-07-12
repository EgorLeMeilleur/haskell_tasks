import Data.Char (digitToInt)

isValidCardNumber :: String -> Bool
isValidCardNumber cardNumber = 
    let reversedCardNumber = reverse cardNumber
        digitCardNumber = map digitToInt reversedCardNumber
        doubledDigitCardNumber = zipWith (*) digitCardNumber (cycle[1,2])
        splitDigits = concatMap split doubledDigitCardNumber
    in sum (splitDigits) `mod` 10 == 0
    where split x = [x `div` 10, x `mod` 10]

test :: Int -> Bool
test ab = 
  let a = "4276490044746479"
      b = "5536914087609875"
      c = "4276480044746479"
      d = "5536914087609874"
  in
      (isValidCardNumber a) == True &&
      (isValidCardNumber b) == True &&
      (isValidCardNumber c) == False &&
      (isValidCardNumber d) == False