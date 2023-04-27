{-# LANGUAGE DataKinds, TypeApplications #-}

module Spec.Settings (testSettings) where

import Amazonka.DynamoDB
import Control.Lens ((?~))
import Data.Generics.Product
import Prosumma.Settings
import RIO
import Test.Hspec

import qualified RIO.HashMap as HM

newRow :: Text -> AttributeValue -> HashMap Text AttributeValue
newRow key value = HM.union
  (HM.singleton "name" (newAttributeValue & (field @"s") ?~ key))
  (HM.singleton "value" value)

testSettings :: Spec
testSettings = do
  describe "settings" $ do
    it "transforms valid settings" $ do
      let stringAttribute = newAttributeValue & (field @"s") ?~ "Vina"
      let row1 = newRow "name" stringAttribute
      let integerAttribute = newAttributeValue & (field @"n") ?~ "22"
      let row2 = newRow "age" integerAttribute
      let rows = [row1, row2]
      let expected = HM.union (HM.singleton "name" (S "Vina")) (HM.singleton "age" (N 22))
      settings rows `shouldBe` expected
    it "skips invalid settings" $ do
      let stringAttribute = newAttributeValue & (field @"s") ?~ "Vina"
      let row1 = newRow "name" stringAttribute
      let integerAttribute = newAttributeValue
      let row2 = newRow "age" integerAttribute
      let rows = [row1, row2]
      let expected = HM.singleton "name" (S "Vina") 
      settings rows `shouldBe` expected