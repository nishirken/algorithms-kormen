module SortSpec where

import Test.Hspec (context, describe, it, SpecWith, shouldBe, shouldThrow, Selector)
import Test.QuickCheck (Property, property, generate, Gen, arbitrary)
import Data.List (sort)
import Sort (
  bubbleSort
  , insertionSort
  , mergeSort
  , quickSort
  )
import Sort.HeapSort (
  heapSort
  , siftDown
  , swap
  , buildMaxHeap
  )
import Control.Exception.Base (ArrayException, evaluate)
import qualified Data.Vector as V
import qualified Control.Monad.ST as ST

testSort f = property $ \xs -> f (xs :: [Int]) `shouldBe` sort xs

sortSpec :: SpecWith ()
sortSpec =
  describe "Sort" $ do
    it "Insertion" $ testSort insertionSort
    it "Merge" $ testSort mergeSort
    it "Bubble" $ testSort bubbleSort

    context "HeapSort" $ do
      context "swap" $ do
        let
          immutableVector = V.fromList [10, 11, 5, 15, 3]
          testSwap :: V.Vector Int -> Int -> Int -> V.Vector Int
          testSwap initial i j =  ST.runST (do
            mutableVector <- V.thaw initial
            swap mutableVector i j
            V.freeze mutableVector)
        it "throws with empty" $ do
          let
            testSwap = ST.runST (do
              emptyVector <- V.thaw (V.empty :: V.Vector Int)
              swap emptyVector 0 0
              V.freeze emptyVector)
          evaluate testSwap `shouldThrow` (const True :: Selector ArrayException)
        it "works with single" $ testSwap (V.singleton 1) 0 0 `shouldBe` V.singleton 1
        it "1" $ testSwap immutableVector 0 4 `shouldBe` V.fromList [3, 11, 5, 15, 10]
        it "2" $ testSwap immutableVector 1 3 `shouldBe` V.fromList [10, 15, 5, 11, 3]
        it "double swap" $ do
          let
            swapped = ST.runST (do
              mutableVector <- V.thaw immutableVector
              swap mutableVector 0 3 -- [15, 11, 5, 10, 3]
              swap mutableVector 0 1
              V.freeze mutableVector)
          swapped `shouldBe` V.fromList [11, 15, 5, 10, 3]
      context "siftDown" $ do
        it "single" $ do
          let
            heap = ST.runST (do
              mutableVector <- V.thaw $ V.singleton 1
              siftDown mutableVector 0 1
              V.freeze mutableVector)
          heap `shouldBe` V.singleton 1
        it "two" $ do
          let
            heap = ST.runST (do
              mutableVector <- V.thaw $ V.fromList [1, 3]
              siftDown mutableVector 0 2
              V.freeze mutableVector)
          heap `shouldBe` V.fromList [3, 1]
        it "three0" $ do
          let
            heap = ST.runST (do
              mutableVector <- V.thaw $ V.fromList [3, 1, 4]
              siftDown mutableVector 0 3
              V.freeze mutableVector)
          heap `shouldBe` V.fromList [4, 1, 3]
        it "three1" $ do
          let
            heap = ST.runST (do
              mutableVector <- V.thaw $ V.fromList [3, 1, 4]
              siftDown mutableVector 1 3
              V.freeze mutableVector)
          heap `shouldBe` V.fromList [3, 1, 4]
        it "three2" $ do
          let
            heap = ST.runST (do
              mutableVector <- V.thaw $ V.fromList [3, 1, 4]
              siftDown mutableVector 2 3
              V.freeze mutableVector)
          heap `shouldBe` V.fromList [3, 1, 4]
        it "full" $ do
          let
            expected = V.fromList [27, 17, 10, 16, 13, 9, 1, 5, 7, 12, 4, 8, 3, 0]
            heap = ST.runST (do
              mutableVector <- V.thaw $ V.fromList [27, 17, 3, 16, 13, 10, 1, 5, 7, 12, 4, 8, 9, 0]
              siftDown mutableVector 2 (V.length expected)
              V.freeze mutableVector)
          heap `shouldBe` expected

      it "buildMaxHeap" $ do
        let
          expected = V.fromList [84, 22, 19, 10, 3, 17, 6, 5, 9]
          heap = ST.runST (do
            mutableVector <- V.thaw $ V.fromList [5, 3, 17, 10, 84, 19, 6, 22, 9]
            buildMaxHeap mutableVector
            V.freeze mutableVector)
        heap `shouldBe` expected

      it "heapSort" $ property $ \xs -> heapSort (xs :: [Int]) `shouldBe` sort xs
      it "Persist data" $ do
        xs <- generate (arbitrary :: Gen [Int])
        let
          ys = (V.toList . V.fromList) xs
          res = heapSort xs
        ys `shouldBe` xs

    it "quickSort" $ testSort quickSort
