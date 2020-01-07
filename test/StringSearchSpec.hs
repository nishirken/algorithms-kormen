module StringSearchSpec where

import Test.Hspec (context, describe, it, Spec, shouldBe)
import Test.QuickCheck (property)
import StringSearch

stringSearchSpec :: Spec
stringSearchSpec = describe "StringSearchSpec" $ do
  context "naive match" $ do
    it "match in empty" $ property $ \xs -> naiveMatch "" (xs <> "1") `shouldBe` False
    it "match empty" $ property $ \xs -> naiveMatch xs "" `shouldBe` True
    it "match in mid" $ property $ \xs -> naiveMatch ("1" <> xs <> "1") xs `shouldBe` True
    it "match in start" $ property $ \xs -> naiveMatch (xs <> "1") xs `shouldBe` True
    it "match in end" $ property $ \xs -> naiveMatch ("1" <> xs) xs `shouldBe` True
