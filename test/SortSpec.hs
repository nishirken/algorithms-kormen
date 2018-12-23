module SortSpec where

import Test.Hspec (context, describe, it, SpecWith, shouldBe, shouldThrow, Selector)
import Test.QuickCheck (Property, property)
import Data.List (sort)
import Sort (
  bubbleSort
  , insertionSort
  , mergeSort
  , heapSort
  , maxHeapify
  , swap
  , buildMaxHeap
  , heapSort
  , quickSort
  )
import Control.Exception.Base (ArrayException, evaluate)
import qualified Data.Vector as V

testSort f = property $ \xs -> f (xs :: [Int]) `shouldBe` sort xs

sortSpec :: SpecWith ()
sortSpec =
  describe "Sort" $ do
    context "swap fn" $ do
      it "throws with empty"
        $ evaluate (swap (V.empty :: V.Vector Int) 0 0) `shouldThrow` (const True :: Selector ArrayException)
      it "works with single" $ swap (V.singleton 1) 0 0 `shouldBe` V.singleton 1
      it "works" $ swap (V.fromList [1, 4, 5, 6, 7]) 1 3 `shouldBe` V.fromList [1, 6, 5, 4, 7]

    it "Insertion" $ testSort insertionSort
    it "Merge" $ testSort mergeSort
    it "Bubble" $ testSort bubbleSort

    context "Heap sort" $ do
      it "maxHeapify" $
        let
          expected = V.fromList [27, 17, 10, 16, 13, 9, 1, 5, 7, 12, 4, 8, 3, 0]
          testVector = V.fromList [27, 17, 3, 16, 13, 10, 1, 5, 7, 12, 4, 8, 9, 0]
          in maxHeapify testVector 2 (length testVector - 1) `shouldBe` expected

      it "buildMaxHeap" $
        let
          expected = V.fromList [84, 22, 19, 10, 3, 17, 6, 5, 9]
          testVector = V.fromList [5, 3, 17, 10, 84, 19, 6, 22, 9]
          in buildMaxHeap testVector `shouldBe` expected

      it "Heap sort" $ property $ \xs -> heapSort (V.fromList (xs :: [Int])) `shouldBe` (V.fromList . sort) xs

    it "quickSort" $ testSort quickSort
