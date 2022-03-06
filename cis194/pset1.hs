-- Convert Integer to its digits (note: fails for input == 0)
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

-- Double every other element
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []         = []     
doubleEveryOther (x:[])     = [x]    
doubleEveryOther (x:(y:zs)) = [2*x, y] ++ doubleEveryOther zs

-- Sum the Digits of Every Integer in the given list
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = sum (toDigits x)
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

-- Validate Credit Card Number
validate :: Integer -> Bool
validate x = sumDigits(doubleEveryOther(toDigits(x))) `mod` 10 == 0

-- Moves n weighted plates from Peg A to Peg B. Tower of Hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a
