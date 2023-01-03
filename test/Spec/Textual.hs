module Spec.Textual (
  testTextual
) where

import RIO
import Test.Hspec

testTextual :: Spec
testTextual = do
  describe "foo" $
    it "bar" $
      2 + 2 `shouldBe` 4