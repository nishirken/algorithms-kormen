module SortSpec where

import Test.Hspec (context, describe, it, SpecWith, shouldBe)
import Test.QuickCheck (property)
import Data.List (sort)
import Sort (bubbleSort, insertionSort, mergeSort, heapSort, maxHeapify, swap)

sortSpec :: SpecWith ()
sortSpec =
  describe "Sort" $ do
    context "swap fn" $ do
      it "works with empty" $ swap ([] :: [Int]) 0 0 `shouldBe` []
      it "works with single" $ swap [1] 0 0 `shouldBe` [1]
      it "works" $ swap [1, 4, 5, 6, 7] 1 3 `shouldBe` [1, 6, 5, 4, 7]

    it "Insertion" $ property $ \x -> insertionSort (x :: [Int]) == sort x
    it "Merge" $ property $ \x -> mergeSort (x :: [Int]) == sort x
    it "Bubble" $ property $ \x -> bubbleSort (x :: [Int]) == sort x
    context "Heap sort" $ do
      it "maxHeapify" $
        (maxHeapify testList 2 (length testList - 1)) `shouldBe` expected
          where
            expected = [27, 17, 10, 16, 13, 9, 1, 5, 7, 12, 4, 8, 3, 0]
            testList = [27, 17, 3, 16, 13, 10, 1, 5, 7, 12, 4, 8, 9, 0]
