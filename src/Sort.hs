module Sort (bubbleSort, insertionSort, mergeSort) where

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []
    where
        insert :: Ord a => a -> [a] -> [a]
        insert x [] = [x]
        insert x acc@(y:ys) = if x > y then y : insert x ys else x : acc

mergeSort :: Ord a => [a] -> [a]
mergeSort xs = _sort xs $ length xs
    where
        merge :: Ord a => [a] -> [a] -> [a]
        merge [] x = x
        merge x [] = x
        merge left@(x:xs) right@(y:ys) = if x < y then x : merge xs right else y : merge left ys
        _sort :: Ord a => [a] -> Int -> [a]
        _sort [x] _ = [x]
        _sort [] _ = []
        _sort xs len =
            let
                half = len `div` 2
                (left, right) = splitAt half xs in
                merge (_sort left half) (_sort right $ half + 1)

bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort (x:xs) = undefined
