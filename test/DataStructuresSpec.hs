module DataStructuresSpec where

import Test.Hspec (context, describe, it, Spec, shouldBe, shouldThrow, Selector)
import Test.QuickCheck (Property, property)
import qualified DataStructures.Stack as S
import qualified DataStructures.Queue as Q

dataStructuresSpec :: Spec
dataStructuresSpec = describe "DataStructuresSpec" $ do
  context "Stack" $ do
    let
      shownEmpty :: S.Stack [Int]
      shownEmpty = S.empty
    it "creates from list" $ S.fromList [1, 2, 3] `shouldBe` S.Stack 3 [1, 2, 3]
    it "creates new" $ shownEmpty `shouldBe` S.fromList []
    it "push" $ S.push (S.fromList [1, 2]) 3 `shouldBe` S.fromList [3, 1, 2]
    it "push with empty" $ (S.push S.empty 3) `shouldBe` S.Stack 1 [3]
    it "pop" $ S.pop (S.fromList [1, 2]) `shouldBe` (S.fromList [2], Just 1)
    it "pop with empty" $ S.pop shownEmpty `shouldBe` (S.empty, Nothing)
    it "isEmpty with empty" $ S.isEmpty shownEmpty `shouldBe` True
    it "isEmpty with not empty" $ S.isEmpty (S.fromList [1]) `shouldBe` False
    it "size with empty" $ S.size shownEmpty `shouldBe` 0
    it "size with not empty" $ S.size (S.fromList [1]) `shouldBe` 1

  context "Queue" $ do
    let
      shownEmpty :: Q.Queue [Int]
      shownEmpty = Q.empty
    it "creates from list" $ Q.fromList [1, 2, 3] `shouldBe` Q.Queue 3 [1, 2, 3]
    it "creates new" $ shownEmpty `shouldBe` Q.fromList []
    it "enqueue" $ Q.enqueue (Q.fromList [1, 2]) 3 `shouldBe` Q.fromList [1, 2, 3]
    it "enqueue with empty" $ (Q.enqueue Q.empty 3) `shouldBe` Q.fromList [3]
    it "dequeue" $ Q.dequeue (Q.fromList [1, 2]) `shouldBe` Just (Q.fromList [2], 1)
    it "dequeue with empty" $ Q.dequeue shownEmpty `shouldBe` Nothing
    it "isEmpty with empty" $ Q.isEmpty shownEmpty `shouldBe` True
    it "isEmpty with not empty" $ Q.isEmpty (Q.fromList [1]) `shouldBe` False
    it "size with empty" $ Q.size shownEmpty `shouldBe` 0
    it "size with not empty" $ Q.size (Q.fromList [1]) `shouldBe` 1
