module DataStructuresSpec where

import Test.Hspec (context, describe, it, Spec, shouldBe, shouldThrow, Selector)
import Control.Exception.Base (ArrayException, evaluate)
import Test.QuickCheck (property)
import qualified DataStructures.Stack as S
import qualified DataStructures.Queue as Q
import qualified DataStructures.LinkedList as L
import qualified DataStructures.BinaryTree as B
import qualified DataStructures.RedBlackTree as RB
import Arbitrary

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

  context "BinaryTree" $ do
    let
      tree = B.Node
        (Just $ B.Node (Just $ B.Node Nothing 2 "2" Nothing) 5 "5" (Just $ B.Node Nothing 5 "5" Nothing))
        6
        "6"
        (Just $ B.Node Nothing 7 "7" (Just (B.Node Nothing 8 "8" Nothing)))
      secondTree = (B.Node (Just $ B.Node Nothing 1 "1" Nothing) 3 "3" (Just $ B.Node Nothing 4 "4" Nothing))
    it "toList" $ map snd (B.toList tree) `shouldBe` ["2", "5", "5", "6", "7", "8"]
    it "toList with one value" $ B.toList (B.Node Nothing 1 "1" Nothing) `shouldBe` [(1, "1")]
    it "size 1" $ B.size tree `shouldBe` 6
    it "size 2" $ B.size secondTree `shouldBe` 3
    it "search 6" $ B.search tree 6 "6" `shouldBe` Just (6, "6")
    it "search 5" $ B.search tree 5 "5" `shouldBe` Just (5, "5")
    it "search 7" $ B.search tree 7 "7" `shouldBe` Just (7, "7")
    it "maximum" $ B.max' tree `shouldBe` "8"
    it "minimum" $ B.min' tree `shouldBe` "2"
    it "insert in single" $ B.insert (B.Node Nothing 1 "1" Nothing) 2 "2" `shouldBe` B.Node Nothing 1 "1" (Just $ B.Node Nothing 2 "2" Nothing)
    it "insert in 3" $ do
      let
        xs = B.Node (Just $ B.Node Nothing 0 "0" Nothing) 1 "1" (Just $ B.Node Nothing 4 "4" Nothing)
        expect = B.Node (Just $ B.Node Nothing 0 "0" Nothing) 1 "1" (Just $ B.Node (Just $ B.Node Nothing 3 "3" Nothing) 4 "4" Nothing)
      B.insert xs 3 "3" `shouldBe` expect
    it "insert toList" $ map snd (B.toList (B.insert tree 7 "7")) `shouldBe` ["2", "5", "5", "6", "7", "7", "8"]
    it "insert max" $ B.max' (B.insert tree 11 "11") `shouldBe` "11"
    it "insert min" $ B.min' (B.insert tree 1 "1") `shouldBe` "1"
    it "insert search" $ property $
      \x y z -> B.search (B.insert (x :: B.BinaryTree Int String) y z) y z `shouldBe` Just (y, z)
    it "insert size" $ property $
      \x y z -> B.size (B.insert (x :: B.BinaryTree Int String) y z) `shouldBe` (B.size x) + 1
    it "join two nodes" $
      B.join (B.Node Nothing 1 "1" Nothing) (B.Node Nothing 2 "2" Nothing) `shouldBe` (B.Node Nothing 1 "1" (Just $ B.Node Nothing 2 "2" Nothing))
    it "join left to right" $ do
      let
        expect = B.Node
          (Just $ B.Node
            (Just $ B.Node (Just $ B.Node Nothing 1 "1" Nothing) 2 "2" (Just $ B.Node Nothing 3 "3" (Just $ B.Node Nothing 4 "4" Nothing)))
            5
            "5"
            (Just $ B.Node Nothing 5 "5" Nothing))
          6
          "6"
          (Just $ B.Node Nothing 7 "7" (Just (B.Node Nothing 8 "8" Nothing)))
      B.join tree secondTree `shouldBe` expect
    it "join size" $ property $
      \x y -> B.size (B.join (x :: B.BinaryTree Int Int) (y :: B.BinaryTree Int Int)) `shouldBe` B.size (B.join y x)
    it "delete toList 5" $ ((map fst) . B.toList <$> (B.delete tree 5)) `shouldBe` Just [2, 5, 6, 7, 8]
    it "delete toList 6" $ ((map fst) . B.toList <$> (B.delete tree 6)) `shouldBe` Just [2, 5, 5, 7, 8]
    it "delete max" $ (B.max' <$> (B.delete tree 8)) `shouldBe` Just "7"
    it "delete min" $ (B.min' <$> (B.delete tree 2)) `shouldBe` Just "5"
    it "invert" $ do
      let
        xs = B.Node
          (Just $ B.Node (Just $ B.Node Nothing 1 "1" Nothing) 2 "2" (Just $ B.Node Nothing 3 "3" Nothing))
          4
          "4"
          (Just $ B.Node (Just $ B.Node Nothing 6 "6" Nothing) 7 "7" (Just $ B.Node Nothing 9 "9" Nothing))
        ys = B.Node
          (Just $ B.Node (Just $ B.Node Nothing 9 "9" Nothing) 7 "7" (Just $ B.Node Nothing 6 "6" Nothing))
          4
          "4"
          (Just $ B.Node (Just $ B.Node Nothing 3 "3" Nothing) 2 "2" (Just $ B.Node Nothing 1 "1" Nothing))
      B.invert xs `shouldBe` ys

  context "RedBlackTree" $ do
    let
      tree = RB.Node
        (Just $ RB.Node (Just $ RB.Node Nothing 1 RB.Red Nothing) 2 RB.Red (Just $ RB.Node Nothing 3 RB.Red (Just $ RB.Node Nothing 4 RB.Red Nothing)))
        6
        RB.Red
        (Just $ RB.Node Nothing 7 RB.Red (Just $ RB.Node Nothing 8 RB.Red Nothing))
    it "search 6" $ RB.search tree 6 `shouldBe` Just 6
    it "search 8" $ RB.search tree 8 `shouldBe` Just 8
    it "search 3" $ RB.search tree 3 `shouldBe` Just 3
    it "search 10" $ RB.search tree 10 `shouldBe` Nothing
    it "min" $ RB.min' tree `shouldBe` 1
    it "max" $ RB.max' tree `shouldBe` 8
