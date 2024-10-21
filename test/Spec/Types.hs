{-# LANGUAGE DeriveGeneric #-}

module Spec.Types (testTypes) where

import Data.Aeson
import Data.CaseInsensitive (CI)
import Data.Default
import Prosumma.Textual
import Prosumma.Types
import RIO
import Test.Hspec

newtype Foo = Foo { foo :: CI Text } deriving (Generic, Eq, Show)

instance FromJSON Foo
instance ToJSON Foo

testTypes :: Spec
testTypes = do 
  describe "Language" $ do
    it "can be initialized from a valid string" $ do
      (fromText "de" :: Maybe Language) `shouldBe` Just "de"
    it "cannot be initialized from an invalid string" $ do
      (fromText "DE" :: Maybe Language) `shouldBe` Nothing
  describe "IANATimeZone" $ do
    it "is textual" $ do
      let iana :: Maybe IANATimeZone = fromText "America/New_York"
      iana `shouldBe` Just "America/New_York"
    it "fails appropriately" $ do 
      let iana :: Maybe IANATimeZone = fromText "$^%"
      iana `shouldBe` Nothing
    it "has a default of Etc/UTC" $ do
      (def :: IANATimeZone) `shouldBe` "Etc/UTC"
  describe "Name" $ do
    it "can be initialized from a valid string" $ do
      (fromText "xyz" :: Maybe Name) `shouldBe` Just "xyz"
    it "cannot be initialized from an invalid string" $ do
      (fromText "0.[" :: Maybe Name) `shouldBe` Nothing
  describe "CI conformances" $ do
    it "should serialize correctly" $ do 
      let f = Foo "foo"
      encode f `shouldBe` "{\"foo\":\"foo\"}"
    it "should deserialize correctly" $ do
      let json = "{\"foo\":\"foo\"}"
      let expected = Foo "foo"
      eitherDecode json `shouldBe` Right expected
