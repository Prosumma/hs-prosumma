module Spec.Types (testTypes) where

import Prosumma.Textual
import Prosumma.Types
import RIO
import Test.Hspec

testTypes :: Spec
testTypes = do 
  describe "IANATimeZone" $ do
    it "is textual" $ do
      let iana :: Maybe IANATimeZone = fromText "America/New_York"
      iana `shouldBe` Just "America/New_York"
    it "fails appropriately" $ do 
      let iana :: Maybe IANATimeZone = fromText "$^%"
      iana `shouldBe` Nothing
