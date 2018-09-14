module BSpec
  ( spec
  ) where

import Test.Hspec

spec :: Spec
spec = describe "Test B Spec" $ it "can pass" $ True `shouldBe` True
