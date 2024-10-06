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

--Exercise 3 

sumDigits :: [Integer] -> Integer
sumDigits nums = sum(map (\n -> (n `div` 10) + (n `mod` 10)) nums)

--Exercise 4 

validate :: Integer -> Bool
validate cardNum = sumDigits (doubleEveryOther (toDigitsRev cardNum)) `mod` 10 == 0

--Example: validate 4012888888881881 = True
--Example: validate 4012888888881882 = False


