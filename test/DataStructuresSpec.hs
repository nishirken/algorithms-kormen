module DataStructuresSpec where

import Test.Hspec (context, describe, it, Spec, shouldBe, shouldThrow, Selector)
import Control.Exception.Base (ArrayException, evaluate)
import qualified DataStructures.Stack as S
import qualified DataStructures.Queue as Q
import qualified DataStructures.LinkedList as L

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

  context "LinkedList" $ do
    let xs = L.LinkedItem (Just (L.LinkedItem (Just (L.LinkedItem Nothing 4 "2")) 2 "44")) 1 "22"
    it "search" $ L.search xs 4 `shouldBe` Just "2"
    it "search with singleton" $ L.search (L.LinkedItem Nothing 1 "2") 1 `shouldBe` Just "2"
    it "search with wrong key" $ L.search xs 22 `shouldBe` Nothing
    it "first" $ L.first xs `shouldBe` "22"
    it "first with singleton" $ L.first (L.LinkedItem Nothing 1 "2") `shouldBe` "2"
    it "rest" $ L.rest xs `shouldBe` Just (L.LinkedItem (Just (L.LinkedItem Nothing 4 "2")) 2 "44")
    it "rest with singleton" $ L.rest (L.LinkedItem Nothing 1 "2") `shouldBe` Nothing
    it "insert" $ (L.first $ L.insert (L.LinkedItem Nothing 1 "2") 2 "4") `shouldBe` "4"
    it "delete in mid" $ L.delete xs 2 `shouldBe` L.LinkedItem (Just (L.LinkedItem Nothing 4 "2")) 1 "22"
    it "delete fst" $ L.delete xs 1 `shouldBe` L.LinkedItem (Just (L.LinkedItem Nothing 4 "2")) 2 "44"
    it "delete last with many items" $ L.delete xs 4 `shouldBe` L.LinkedItem (Just (L.LinkedItem Nothing 2 "44")) 1 "22"
    it "delete last with one item" $ (evaluate $ L.delete (L.LinkedItem Nothing 4 "22") 4) `shouldThrow` (const True :: Selector ArrayException)
    it "join with singleton fst" $ L.join (L.LinkedItem Nothing 1 "1") xs `shouldBe` L.LinkedItem (Just xs) 1 "1"
    it "join" $ do
      let
        xs' = L.LinkedItem (Just (L.LinkedItem Nothing 22 "1")) 11 "1"
        expect = L.LinkedItem (Just (L.LinkedItem (Just xs) 22 "1")) 11 "1"
      L.join xs' xs `shouldBe` expect
