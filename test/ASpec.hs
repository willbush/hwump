module ASpec
  ( spec
  ) where

import Test.Hspec

spec :: Spec
spec = describe "Test B Spec" $ it "it can pass" $ True `shouldBe` True
