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

instance FromTableItem Watusi where
  fromTableItem = readTableItem $ \read -> Watusi <$> read "foo" <*> read "bar" <*> read "baz"

instance ToTableItem Watusi where
  toTableItem Watusi{..} = writeTableItem [
      "foo" =: watusiFoo,
      "bar" =: watusiBar,
      "baz" =: watusiBaz
    ]

testDynamoDB :: Spec
testDynamoDB = do
  describe "readTableItem" $ do
    it "reads a table item" $ do
      let watusi = Watusi "cool" 35 (Just "baz")
      let item = toTableItem watusi 
      let result = fromTableItem item
      result `shouldBe` Right watusi 
    it "gives an error if a key is not found" $ do
      let item = HM.empty 
      let result :: Either ValueError Watusi = fromTableItem item
      result `shouldBe` Left (ValueMissing (Just "foo"))
    it "gives an error if a key is found but is not the correct type" $ do
      let item = HM.fromList [("foo", BOOL False)]
      let result :: Either ValueError Watusi = fromTableItem item
      result `shouldBe` Left (ValueInvalid (Just "foo") (BOOL False) Nothing) 