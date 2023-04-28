{-# LANGUAGE DataKinds, TypeApplications #-}

module Spec.Settings (testSettings) where

import Amazonka.DynamoDB
import Control.Lens ((?~))
import Data.Generics.Product
import Prosumma.Settings
import Prosumma.Util
import RIO
import Test.Hspec

import qualified RIO.HashMap as HM

data Settings = Settings {
  stgsName :: !Text,
  stgsAge  :: !Int,
  stgsGood :: !Bool,
  stgsWut  :: !(Maybe Int),
  stgsWho  :: !Text 
} deriving (Eq, Show)

readPersonSettings :: [HashMap Text AttributeValue] -> Maybe Settings
readPersonSettings items = readSettings items $ \lookup ->
  Settings
    <$> lookup "name"
    <*> lookup "age"
    <*> lookup "good"
    <*> (lookup "wut" ??~ Nothing)
    <*> (lookup "who" ??~ "Greg")

newRow :: Text -> AttributeValue -> HashMap Text AttributeValue
newRow key value = let attributeKey = newAttributeValue & (field @"s") ?~ key in
  HM.singleton "name" attributeKey <> HM.singleton "value" value

testSettings :: Spec
testSettings = do
  describe "settings" $ do
    it "transforms valid settings into Hashmap Text Setting" $ do
      let stringAttribute = newAttributeValue & (field @"s") ?~ "Vina"
      let row1 = newRow "name" stringAttribute
      let integerAttribute = newAttributeValue & (field @"n") ?~ "22"
      let row2 = newRow "age" integerAttribute
      let boolAttribute = newAttributeValue & (field @"bool") ?~ False
      let row3 = newRow "good" boolAttribute
      let rows = [row1, row2, row3]
      let expected = HM.singleton "name" (S "Vina") <> HM.singleton "age" (N 22) <> HM.singleton "good" (B False)
      settings rows `shouldBe` expected
    it "skips invalid settings" $ do
      let stringAttribute = newAttributeValue & (field @"s") ?~ "Vina"
      let row1 = newRow "name" stringAttribute
      let integerAttribute = newAttributeValue
      let row2 = newRow "age" integerAttribute
      let rows = [row1, row2]
      let expected = HM.singleton "name" (S "Vina") 
      settings rows `shouldBe` expected
  describe "readSetting" $ do 
    it "assists in initializing records" $ do
      let stringAttribute = newAttributeValue & (field @"s") ?~ "Vina"
      let row1 = newRow "name" stringAttribute
      let integerAttribute = newAttributeValue & (field @"n") ?~ "22"
      let row2 = newRow "age" integerAttribute
      let boolAttribute = newAttributeValue & (field @"bool") ?~ False
      let row3 = newRow "good" boolAttribute
      let rows = [row1, row2, row3]
      let expected = Settings "Vina" 22 False Nothing "Greg" 
      readPersonSettings rows `shouldBe` Just expected 
    it "fails to initialize a record given invalid data" $ do
      let stringAttribute = newAttributeValue & (field @"s") ?~ "Vina"
      let row1 = newRow "name" stringAttribute
      let integerAttribute = newAttributeValue
      let row2 = newRow "age" integerAttribute
      let rows = [row1, row2]
      readPersonSettings rows `shouldBe` Nothing 