module SortSpec where

import Test.Hspec (context, describe, it, SpecWith)
import Test.QuickCheck (property)
import Data.List (sort)
import Sort (bubbleSort, insertionSort, mergeSort, maxHeapify, Heap (..))

sortSpec :: SpecWith ()
sortSpec =
  describe "Sort" $ do
    it "Insertion" $ property $ \x -> insertionSort (x :: [Int]) == sort x
    it "Merge" $ property $ \x -> mergeSort (x :: [Int]) == sort x
    it "Bubble" $ property $ \x -> bubbleSort (x :: [Int]) == sort x

    context "Heap sort" $ do
      it "maxHeapify" $ maxHeapify testHeap 4
        where
          testHeap :: Heap Int
          testHeap = Heap {
            value = 19
            , left = Heap {
              value = 14
              , left = Heap { value = 8, left = Empty, right = Empty }
              , right = Heap { value = 7, left = Empty, right = Empty }
            }
            , right = Heap {
              value = 10
              , left = Heap { value = 9, left = Empty, right = Empty }
              , right = Heap { value = 3, left = Empty, right = Empty }
            }
          }
