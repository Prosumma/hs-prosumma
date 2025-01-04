{-# LANGUAGE DeriveGeneric #-}

module Spec.Types (testTypes) where

import Data.Aeson
import Data.Default
import Prosumma.Textual
import Prosumma.Types.IP
import Prosumma.Types.Localization
import Prosumma.Types.TimeZone
import Prosumma.Util
import RIO
import RIO.Partial
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
  describe "Region" $ do
    it "can be initialized from a valid string" $ do
      (fromText "DE" :: Maybe Region) `shouldBe` Just "DE"
    it "cannot be initialized from an invalid string" $ do
      (fromText "de" :: Maybe Region) `shouldBe` Nothing
  describe "Localization" $ do
    it "can be initialized from a valid string" $ do
      (fromText "de-DE" :: Maybe Localization) `shouldBe` Just "de-DE"
    it "cannot be initialized from an invalid string" $ do
      (fromText "en-DEx" :: Maybe Localization) `shouldBe` Nothing
  describe "IANATimeZone" $ do
    it "is textual" $ do
      let iana :: Maybe IANATimeZone = fromText "America/New_York"
      iana `shouldBe` Just "America/New_York"
    it "fails appropriately" $ do 
      let iana :: Maybe IANATimeZone = fromText "$^%"
      iana `shouldBe` Nothing
    it "has a default of Etc/UTC" $ do
      (def :: IANATimeZone) `shouldBe` "Etc/UTC"
  describe "CI conformances" $ do
    it "should serialize correctly" $ do 
      let f = Foo "foo"
      encode f `shouldBe` "{\"foo\":\"foo\"}"
    it "should deserialize correctly" $ do
      let json = "{\"foo\":\"foo\"}"
      let expected = Foo "foo"
      eitherDecode json `shouldBe` Right expected
  describe "IP" $ do
    it "should serialize correctly" $ do
      let ip = fromText "192.168.1.1" :: Maybe IP
      let expected = "\"192.168.1.1\""
      encode ip `shouldBe` expected 
    it "should deserialize correctly" $ do
      let json = "\"192.168.1.1\""
      let expected = fromJust (fromText "192.168.1.1") :: IP
      eitherDecode json `shouldBe` Right expected
