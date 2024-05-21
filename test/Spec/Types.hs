{-# LANGUAGE DeriveGeneric #-}

module Spec.Types (testTypes) where

import Data.CaseInsensitive (CI)
import Data.Aeson
import Prosumma.Textual
import Prosumma.Types
import RIO
import Test.Hspec

import qualified Data.CaseInsensitive as CI

newtype Foo = Foo { foo :: CI Text } deriving (Generic, Eq, Show)

instance FromJSON Foo
instance ToJSON Foo

testTypes :: Spec
testTypes = do 
  describe "IANATimeZone" $ do
    it "is textual" $ do
      let iana :: Maybe IANATimeZone = fromText "America/New_York"
      iana `shouldBe` Just "America/New_York"
    it "fails appropriately" $ do 
      let iana :: Maybe IANATimeZone = fromText "$^%"
      iana `shouldBe` Nothing
  describe "CI conformances" $ do
    it "should serialize correctly" $ do 
      let f = Foo "foo"
      encode f `shouldBe` "{\"foo\":\"foo\"}"
    it "should deserialize correctly" $ do
      let json = "{\"foo\":\"foo\"}"
      let expected = Foo "foo"
      eitherDecode json `shouldBe` Right expected
