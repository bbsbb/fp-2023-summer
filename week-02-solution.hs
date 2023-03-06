-- Haskell: Week 02

sumTwoNumbers :: Int -> Int -> Int
sumTwoNumbers 1 _ = 14
sumTwoNumbers a b = a + b

-- Factorial with pattern matchin, if & guards.

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial(n - 1)


factorialGuard :: Integer -> Integer
factorialGuard n
  | n == 0 = 1
  | otherwise = n * factorialGuard(n - 1)


factorialIf :: Integer -> Integer
factorialIf n = if n <= 1
                then 1
                else n * (factorialIf (n - 1))

countDigits :: Int -> Int
countDigits n
  | n < 10 = 1
  | otherwise = 1 + countDigits (n `div` 10)


allAfterN :: Int -> [Int]
allAfterN n = n:allAfterN(n + 1)
