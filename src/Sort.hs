module Sort (insertionSort) where

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort xs = foldr insert [] xs
    where
        insert :: Ord a => a -> [a] -> [a]
        insert x [] = [x]
        insert x acc@(y:ys) = if x > y then y : insert x ys else x : acc
