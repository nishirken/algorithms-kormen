module DataStructuresSpec where

import Test.Hspec (context, describe, it, Spec, shouldBe, shouldThrow, Selector)
import Test.QuickCheck (Property, property)
import qualified DataStructures.Stack as S

dataStructuresSpec :: Spec
dataStructuresSpec = describe "StackSpec" $ do
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
