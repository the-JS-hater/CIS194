-- Excercise 1

toDigits :: Integer -> [Integer]
toDigits n  
  | n <= 0 = []
  | otherwise = toDigits ((n - (n `mod`  10)) `div` 10) ++ [(n `mod` 10)] 

toDigitsRev :: Integer -> [Integer]
toDigitsRev n  
  | n <= 0 = []
  | otherwise = [(n `mod` 10)] ++ toDigits ((n - (n `mod`  10)) `div` 10) 

--Exercise 2 

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x : xs : xss) = [x, xs * 2] ++ (doubleEveryOther xss)
