module Sort (bubbleSort, insertionSort, mergeSort, maxHeapify, buildMaxHeap, heapSort, swap) where

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

left :: Index -> Index
left i = 2 * i + 1

right :: Index -> Index
right i = 2 * i + 2

swap :: Eq a => [a] -> Index -> Index -> [a]
swap [] _ _ = []
swap [x] _ _ = [x]
swap xs index newIndex =
  if correct index && correct newIndex && index < newIndex
    then left ++ [newItem] ++ middle ++ [item] ++ right
    else error $ "Incorrect index in replace" ++ show (index, newIndex)
      where
        correct i = i < length xs && i >= 0
        item = xs !! index
        newItem = xs !! newIndex
        left = take index xs
        middle = take (newIndex - index - 1) (drop (index + 1) xs)
        right = drop (newIndex + 1) xs

maxHeapify :: Ord a => [a] -> Index -> Int -> [a]
maxHeapify heap i heapSize =
  if i == largest
    then heap
    else maxHeapify (swap heap i largest) largest heapSize
      where
        l = left i
        r = right i
        getLargest :: Index -> Index -> Index
        getLargest childIndex currentIndex =
          if childIndex <= heapSize && ((heap !! childIndex) > (heap !! currentIndex))
            then childIndex
            else currentIndex
        largest = getLargest r $ getLargest l i

buildMaxHeap :: Ord a => [a] -> [a]
buildMaxHeap xs = iter xs (div (length xs - 1) 2)
  where
    iter :: Ord a => [a] -> Int -> [a]
    iter ys 0 = ys
    iter ys len = iter (maxHeapify ys (len - 1) (length xs - 1)) (len - 1)

heapSort :: Ord a => [a] -> [a]
heapSort xs = iter heap (length xs - 1) (length xs - 1)
  where
    heap = buildMaxHeap xs
    iter :: Ord a => [a] -> Int -> Int -> [a]
    iter acc 1 _ = acc
    iter acc i heapSize = iter (maxHeapify (swap acc 0 i) 1 heapSize) (i - 1) (heapSize - 1)
