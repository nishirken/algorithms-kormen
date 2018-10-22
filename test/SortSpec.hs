module SortSpec where

import Test.Hspec (describe, it, SpecWith)
import Test.QuickCheck (property)
import Data.List (sort)
import Sort (insertionSort)

sortSpec :: SpecWith ()
sortSpec =
    describe "Sort" $ do
        it "Insertion" $ property $ \x -> insertionSort (x :: [Int]) == sort x 
