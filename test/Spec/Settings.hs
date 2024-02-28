{-# LANGUAGE DataKinds #-}

module Spec.Settings (testSettings) where

import Amazonka.DynamoDB
import Prosumma.AWS.DynamoDB
import Prosumma.Settings
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

instance FromItem Settings where
  fromItem = readItem $ \read ->
    Settings <$> read "name" <*> read "age" <*> read "good" <*> read "wut" <*> (read "who" <|> pure "Greg")

newRow :: Text -> AttributeValue -> HashMap Text AttributeValue
newRow key value = let attributeKey = S key in
  HM.singleton "name" attributeKey <> HM.singleton "value" value

testSettings :: Spec
testSettings = do
  describe "readSetting" $ do
    it "assists in initializing records" $ do
      let stringAttribute = S "Vina"
      let row1 = newRow "name" stringAttribute
      let integerAttribute = N "22"
      let row2 = newRow "age" integerAttribute
      let boolAttribute = BOOL False
      let row3 = newRow "good" boolAttribute
      let rows = [row1, row2, row3]
      let expected = Settings "Vina" 22 False Nothing "Greg"
      fromItem (settings rows) `shouldBe` Right expected
    it "fails to initialize a record given invalid data" $ do
      let stringAttribute = S "Vina"
      let row1 = newRow "name" stringAttribute
      let integerAttribute = S "Whatever"
      let ageName = "age"
      let row2 = newRow ageName integerAttribute
      let rows = [row1, row2]
      let setting :: Either ItemError Settings = fromItem $ settings rows
      setting `shouldBe` Left (ItemInvalidFormat "age" (Just (S "Whatever")))