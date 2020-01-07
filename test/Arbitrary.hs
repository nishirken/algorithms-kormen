module Arbitrary where

import Test.QuickCheck (Property, property, Arbitrary, arbitrary, sized, choose, suchThat, Gen)
import qualified DataStructures.BinaryTree as B

instance (Show a, Ord a, Arbitrary a, Show b, Eq b, Arbitrary b) => Arbitrary (B.BinaryTree a b) where
  arbitrary = do
    x <- arbitrary
    sized $ makeTree x True
      where
        emptyNode key value = B.Node Nothing key value Nothing
        keyGenerator parent isLeft = suchThat arbitrary $ \x' -> if isLeft then x' < parent else x' >= parent
        makeTree :: (Ord a, Show a, Arbitrary a, Show b, Eq b, Arbitrary b) => a -> Bool -> Int -> Gen (B.BinaryTree a b)
        makeTree parent isLeft 0 = do
          x <- keyGenerator parent isLeft
          y <- arbitrary
          pure $ emptyNode x y
        makeTree parent isLeft n | n > 0 = do
          x <- keyGenerator parent isLeft
          y <- arbitrary
          left <- makeTree x (x < parent) (n `div` 2)
          right <- makeTree x (x >= parent) (n `div` 2)
          pure $ B.Node (Just left) x y (Just right)
