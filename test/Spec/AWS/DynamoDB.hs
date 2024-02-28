{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Spec.AWS.DynamoDB (testDynamoDB) where

import Amazonka.DynamoDB
import Prosumma.AWS.DynamoDB
import RIO
import Test.Hspec

import qualified RIO.HashMap as HM

data Watusi = Watusi {
  watusiFoo :: !Text,
  watusiBar :: !Int,
  watusiBaz :: !(Maybe Text)
} deriving (Eq, Show)

instance FromItem Watusi where
  fromItem = readItem $ \read -> Watusi <$> read "foo" <*> read "bar" <*> read "baz"

instance ToItem Watusi where
  toItem Watusi{..} = HM.fromList [
      "foo" =: watusiFoo,
      "bar" =: watusiBar,
      "baz" =: watusiBaz
    ]

testDynamoDB :: Spec
testDynamoDB = do
  describe "readTableItem" $ do
    it "reads a table item" $ do
      let watusi = Watusi "cool" 35 (Just "baz")
      let item = toItem watusi 
      let result = fromItem item
      result `shouldBe` Right watusi 
    it "gives an error if a key is not found" $ do
      let item = HM.empty 
      let result :: Either ItemError Watusi = fromItem item
      result `shouldBe` Left (ItemMissingValue "foo")
    it "gives an error if a key is found but is not the correct type" $ do
      let item = HM.fromList [("foo", BOOL False)]
      let result :: Either ItemError Watusi = fromItem item
      result `shouldBe` Left (ItemInvalidFormat "foo" (Just (BOOL False))) 