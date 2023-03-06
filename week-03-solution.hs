myDigits :: Int -> [Int]
myDigits n
  | n < 10 = [n]
  | otherwise = (myDigits (n `div` 10)) ++ [n `rem` 10]


isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

-- isPrefix "cat" "cattaaaarday" ->> True

isContained :: String -> String -> Bool
isContained [] _ = True
isContained _ [] = False
isContained xs (y:ys) = isPrefix xs (y:ys) || isContained xs ys

isPrime :: Int -> Bool
isPrime n = [m | m <- [2..(n-1)], n `rem` m == 0] == []

-- Number that is equal

-- 153 == 1^3 + 5^3 + 3^3

-- isNarcissistic 153 --> True

isNarcissistic :: Int -> Bool
isNarcissistic n = n == sum [m ^ exponent | m <- digits] where
  digits = myDigits n
  exponent = length digits
