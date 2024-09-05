-- Exercise 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | n < 10 = [n]
    | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' (x:xl) = reverse' xl ++ [x]

toDigits x = reverse' (toDigitsRev x)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (f:s:xl) = f:(s*2):doubleEveryOther xl

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xl) = sumInteger x + sumDigits xl

sumInteger :: Integer -> Integer
sumInteger x
    | x < 10 = x
    | otherwise = (x `mod` 10) + sumInteger (x `div` 10)


testCase1 = 4012888888881881
testCase2 = 4012888888881882

-- Exercise 4
isZero x = x == 0 
mod10 x = mod x 10
validate :: Integer -> Bool
validate = isZero . mod10 . sumDigits . doubleEveryOther . toDigitsRev
