-- 
toDigits :: Integer -> [Integer]
toDigits x = [x `mod` 10] ++ toDigits (x `div` 10)

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
