module Sort (bubbleSort, insertionSort, mergeSort, heapSort) where

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
bubbleSort xs = outer xs 0
    where
        outer ys counter = if counter == length ys then ys else outer (inner ys) (counter + 1)
        inner [z] = [z]
        inner (z:z':zs) = if z > z' then z' : inner (z:zs) else z : inner (z':zs)

-- Heap sort

type Index = Int

parent :: Index -> Index
parent i = div i 2

left :: Index -> Index
left i = 2 * i

right :: Index -> Index
right i = 2 * i + 1

maxHeapify :: Ord a => [a] -> a -> [a]
maxHeapify heap = undefined

heapSort :: Ord a => [a] -> [a]
heapSort = undefined
