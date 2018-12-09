module SortSpec where

import Test.Hspec (describe, it, SpecWith)
import Test.QuickCheck (property)
import Data.List (sort)
import Sort (bubbleSort, insertionSort, mergeSort, heapSort)

sortSpec :: SpecWith ()
sortSpec =
    describe "Sort" $ do
        it "Insertion" $ property $ \x -> insertionSort (x :: [Int]) == sort x
        it "Merge" $ property $ \x -> mergeSort (x :: [Int]) == sort x
        it "Bubble" $ property $ \x -> bubbleSort (x :: [Int]) == sort x
        it "Heap sort" $ property $ \x -> heapSort (x :: [Int]) == sort x
