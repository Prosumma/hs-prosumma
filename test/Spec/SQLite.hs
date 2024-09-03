module Spec.SQLite where

import RIO
import Test.Hspec

testSQLite :: Spec
testSQLite = describe "foo" $ do
  it "bars" $ do
    True `shouldBe` True