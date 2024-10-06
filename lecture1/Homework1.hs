-- CREDIT CARD NUMBER VALIDATION

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
doubleEveryOther [n] = [n]
doubleEveryOther (x : xs : xss) = [x, xs * 2] ++ (doubleEveryOther xss)

--Exercise 3 

sumDigits :: [Integer] -> Integer
sumDigits nums = sum(map (\n -> (n `div` 10) + (n `mod` 10)) nums)

--Exercise 4 

validate :: Integer -> Bool
validate cardNum = sumDigits (doubleEveryOther (toDigitsRev cardNum)) `mod` 10 == 0

--Example: validate 4012888888881881 = True
--Example: validate 4012888888881882 = False

-- TOWERS OF HANOI

--Exercise 4

--To move n discs (stacked in increasing size) from peg a to peg b
--using peg c as temporary storage,
--1. move n − 1 discs from a to c using b as temporary storage
--2. move the top disc from a to b
--3. move n − 1 discs from c to b using a as temporary storage.

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 1 = [(a, b)]
  | otherwise = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

--Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]

