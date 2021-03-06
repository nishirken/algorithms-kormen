module PrisonersSpec where

import Test.Hspec (context, describe, it, Spec, shouldBe)
import Test.QuickCheck (property)
--import Prisoners
import Numeric.Natural (Natural)

--prisonersSpec :: Spec
--prisonersSpec = describe "Prisoners" $ do
--  it "answer for initial off" $ property $ \x -> do
--    visits <- answer x
--    pure $ all (\visit -> visit >= 1) visits `shouldBe` True
