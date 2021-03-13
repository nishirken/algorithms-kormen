{-# LANGUAGE BangPatterns #-}

module Sort (
  bubbleSort
  , insertionSort
  , mergeSort
  , heapSort
  , quickSort
  , optimalSort
  ) where

import Sort.HeapSort (heapSort)

import Control.Exception.Base (ArrayException (IndexOutOfBounds), throw)
import Data.List (foldl', partition)
import qualified Data.Vector as V

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldl' insert []
  where
    insert :: Ord a => [a] -> a -> [a]
    insert [] x = [x]
    insert !acc@(y:ys) x = if x > y then y : (insert ys x) else x : acc

mergeSort :: Ord a => [a] -> [a]
mergeSort xs = _sort xs $ length xs
  where
    merge :: Ord a => [a] -> [a] -> [a]
    merge [] x = x
    merge x [] = x
    merge !left@(x:xs) !right@(y:ys) = if x < y then x : merge xs right else y : merge left ys
    _sort :: Ord a => [a] -> Int -> [a]
    _sort [x] _ = [x]
    _sort [] _ = []
    _sort xs len =
      let
        !half = len `div` 2
        !(left, right) = splitAt half xs in
        merge (_sort left half) (_sort right $ half + 1)

bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = outer xs 0
  where
    outer !ys counter = if counter == length ys then ys else outer (inner ys) (counter + 1)
    inner [z] = [z]
    inner !(z:z':zs) = if z > z' then z' : inner (z:zs) else z : inner (z':zs)

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

optimalSort :: Ord a => [a] -> [a]
optimalSort [] = []
optimalSort [x] = [x]
optimalSort [x, y] = if x < y then [x, y] else [y, x]
optimalSort xs@[x, y, z] = insertionSort xs
optimalSort xs@[x, y, z, z'] = insertionSort xs
optimalSort xs = quickSort xs
