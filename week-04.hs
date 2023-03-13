-- What we saw last time?

elemAtIndex :: [a] -> Int -> a
elemAtIndex [] _ = error "Poop"
elemAtIndex (x:_) 0 = x
elemAtIndex (x:xs) n = elemAtIndex xs (n - 1)

elemInList :: (Eq a) => [a] -> a -> Bool
elemInList [] _ = False
elemInList (x:xs) y = (x == y) || elemInList xs y

occurencesMin :: [Int] -> Int
occurencesMin xs = length [x | x <- xs, x == m] where
  m = minimum xs

-- [37,65,165,222] [] -> [37,65,165,222]
mergeLists :: (Ord a) => [a] -> [a]  -> [a]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys) = if x < y
                           then x:mergeLists xs (y:ys)
                           else y:mergeLists (x:xs) ys


mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =  mergeLists (mergeSort f) (mergeSort s) where
  (f, s) = splitAt (div (length xs) 2) xs
