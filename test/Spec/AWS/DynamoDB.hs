{-# LANGUAGE RankNTypes #-}

module Spec.AWS.DynamoDB (testDynamoDB) where

import Amazonka.DynamoDB
import Prosumma.AWS.DynamoDB
import Prosumma.Util
import RIO
import Test.Hspec

import qualified RIO.HashMap as HM

data Watusi = Watusi {
  watusiFoo :: !Text,
  watusiBar :: !Int,
  watusiBaz :: !(Maybe Text)
} deriving (Eq, Show)

makeWatusi :: (forall v. ReadAttributeValueByKey v) -> Either String Watusi
makeWatusi read = Watusi <$> read "foo" <*> read "bar" <*> (read "baz" ??~ Nothing)

readWatusi :: TableItem -> Either String Watusi
readWatusi = readTableItem makeWatusi 

readWatusiWithIndex :: Int -> TableItem -> Either String Watusi
readWatusiWithIndex = readTableItemWithIndex makeWatusi

testDynamoDB :: Spec
testDynamoDB = do
  describe "readTableItem" $ do
    it "reads a table item" $ do
      let item = HM.fromList [("foo", S "cool"), ("bar", N "35"), ("baz", S "baz")]
      let result = readWatusi item
      result `shouldBe` Right (Watusi "cool" 35 (Just "baz"))
    it "gives an error if a key is not found" $ do
      let item = HM.empty 
      let result = readWatusi item
      result `shouldBe` Left "The key 'foo' was not found." 
    it "gives an error if a key is found but is not the correct type" $ do
      let item = HM.fromList [("foo", BOOL False)]
      let result = readWatusi item
      result `shouldBe` Left "The key 'foo' was found but was not the correct type."
  describe "readTableItemWithIndex" $ do
    it "reads a table item" $ do
      let item = HM.fromList [("foo", S "cool"), ("bar", N "35"), ("baz", S "baz")]
      let result = readWatusiWithIndex 0 item
      result `shouldBe` Right (Watusi "cool" 35 (Just "baz"))
    it "indicates the index when an error occurs" $ do
      let item = HM.empty 
      let result = readWatusiWithIndex 2 item
      result `shouldBe` Left "At index 2: The key 'foo' was not found." 
