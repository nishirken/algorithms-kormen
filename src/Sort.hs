module Sort (
  bubbleSort
  , insertionSort
  , mergeSort
  , maxHeapify
  , buildMaxHeap
  , heapSort'
  , heapSort
  , swap
  , quickSort
  ) where

import Control.Exception.Base (ArrayException (IndexOutOfBounds), throw)
import Data.List (foldl', partition)
import qualified Data.Vector as V
import qualified Data.Heap as H

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldl' insert []
  where
    insert :: Ord a => [a] -> a -> [a]
    insert [] x = [x]
    insert acc@(y:ys) x = if x > y then y : (insert ys x) else x : acc

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

-- unefficient realisation, even with vectors. 

type Index = Int

left :: Index -> Index
left i = 2 * i + 1

right :: Index -> Index
right i = 2 * i + 2

swap :: Eq a => V.Vector a -> Index -> Index -> V.Vector a
swap xs index newIndex =
  if correct index && correct newIndex && index <= newIndex
    then V.unsafeUpd xs [(index, V.unsafeIndex xs newIndex), (newIndex, V.unsafeIndex xs index)]
    else throw $ IndexOutOfBounds $ "Incorrect index in swap" ++ show (index, newIndex)
      where correct i = i < V.length xs && i >= 0

maxHeapify :: Ord a => V.Vector a -> Index -> Int -> V.Vector a
maxHeapify heap i heapSize =
  if i == largest
    then heap
    else maxHeapify (swap heap i largest) largest heapSize
      where
        l = left i
        r = right i
        getLargest :: Index -> Index -> Index
        getLargest childIndex currentIndex =
          if childIndex <= heapSize && ((heap V.! childIndex) > (heap V.! currentIndex))
            then childIndex
            else currentIndex
        largest = getLargest r $ getLargest l i

buildMaxHeap :: Ord a => V.Vector a -> V.Vector a
buildMaxHeap xs = iter xs (div (V.length xs) 2)
  where
    iter :: Ord a => V.Vector a -> Int -> V.Vector a
    iter ys 0 = ys
    iter ys len =
      let next = len - 1 in iter (maxHeapify ys next (V.length xs - 1)) next

heapSort' :: Ord a => V.Vector a -> V.Vector a
heapSort' xs = if V.null xs then xs
  else iter heap (V.length xs - 1)
    where
      heap = buildMaxHeap xs
      iter :: Ord a => V.Vector a -> Int -> V.Vector a
      iter acc 0 = acc
      iter acc i = iter (maxHeapify (swap acc 0 i) 0 (i - 1)) (i - 1)

-- more efficient, but much less than quickSort

heapSort :: Ord a => [a] -> [a]
heapSort = H.sort

-- quick sort

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort lt ++ eq ++ quickSort gt
  where
    partition lt eq gt [] = (lt, eq, gt)
    partition lt eq gt (y:ys) = case x `compare` y of
      LT -> partition lt eq (y:gt) ys
      EQ -> partition lt (y:eq) gt ys
      GT -> partition (y:lt) eq gt ys
    (lt, eq, gt) = partition [] [x] [] xs
