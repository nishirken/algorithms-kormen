{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Sort.HeapSort where

import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import Control.Exception.Base (ArrayException (IndexOutOfBounds), AssertionFailed (..), throw)
import qualified Control.Monad.ST as ST
import qualified Data.Primitive.Array as Arr
import Control.Monad (forM)
import Debug.Trace
import STVectorShow (showMVector)

type Index = Int

left :: Index -> Index
left i = 2 * i + 1

right :: Index -> Index
right i = 2 * i + 2

swap :: forall a s. Eq a => MV.STVector s a -> Index -> Index -> ST.ST s ()
swap xs oldIndex newIndex =
  if correct oldIndex && correct newIndex
    then unsafeSwap xs oldIndex newIndex
    else throw $ IndexOutOfBounds $ "Incorrect index in swap" ++ show (oldIndex, newIndex)
      where
        correct i = i < MV.length xs && i >= 0
        unsafeSwap vec old new = do
          temp <- MV.read vec old
          MV.read vec new >>= MV.write vec old
          MV.write vec new temp

siftDown :: forall a s. Ord a => MV.STVector s a -> Index -> Int -> ST.ST s ()
siftDown heap i heapSize =
  let
    read :: Index -> ST.ST s a
    read = MV.read heap
    leftIndex = left i
    isLeaf = leftIndex >= heapSize in
  if isLeaf || MV.null heap then pure () else (do
    current <- read i
    leftChild <- read leftIndex
    largest <- do
      let
        rightIndex = right i
        currentOrLeft = if leftChild > current then leftIndex else i
        hasOnlyLeft = rightIndex >= heapSize
      if hasOnlyLeft then pure currentOrLeft else do
        rightChild <- read rightIndex
        let
          currentOrRight = if rightChild > current then rightIndex else i
        pure $ if rightChild > leftChild then currentOrRight else currentOrLeft
    if largest == i then pure () else do
      swap heap i largest
      siftDown heap largest heapSize)

buildMaxHeap :: forall a s. Ord a => MV.STVector s a -> ST.ST s ()
buildMaxHeap xs =
  iter (xsSize `div` 2)
  where
    xsSize = MV.length xs
    iter :: Int -> ST.ST s ()
    iter 0 = pure ()
    iter len =
      let next = len - 1 in do
        siftDown xs next xsSize
        iter next

heapSort :: forall a. (Show a, Ord a) => [a] -> [a]
heapSort [] = []
heapSort [x] = [x]
heapSort [x, y] = if x < y then [x, y] else [y, x]
heapSort xs = V.toList $ ST.runST $ do
  mutableVector <- V.thaw $ V.fromList xs
  buildMaxHeap mutableVector
  let
    vecSize = MV.length mutableVector
    iter 0 heapSize = pure ()
    iter i heapSize = do
      let
        newHeapSize = heapSize - 1
      beforeSwap <- showMVector mutableVector
      swap mutableVector 0 i
      siftDown mutableVector 0 newHeapSize
      iter (i - 1) newHeapSize
  iter (vecSize - 1) vecSize
  V.unsafeFreeze mutableVector
