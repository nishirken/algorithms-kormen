module SortSpec where

import Test.Hspec (describe, it, SpecWith)
import Test.QuickCheck (property)
import Data.List (sort)
import Sort (insertionSort, mergeSort)

sortSpec :: SpecWith ()
sortSpec =
    describe "Sort" $ do
        it "Insertion" $ property $ \x -> insertionSort (x :: [Int]) == sort x
        it "Merge" $ property $ \x -> mergeSort (x :: [Int]) == sort x
