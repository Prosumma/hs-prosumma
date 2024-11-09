{-# LANGUAGE RecordWildCards #-}

module Spec.Push.APNS (
  testAPNS
) where
  
import Control.Lens ((?~))
import Data.Aeson
import Prosumma.Aeson
import Prosumma.Push.APNS
import RIO
import Test.Hspec

data Custom = Custom {
  something :: !Text,
  someone   :: !Int
} deriving (Eq, Show)

instance ToJSONPairs Custom where
  toJSONPairs Custom{..} = [ "something" .= something, "someone" .= someone ]

testAPNS :: Spec
testAPNS = do
  describe "newNotification" $ do
    it "creates a new notification" $ do
      let n = newNotification "Hello!"
      encode n `shouldBe` "{\"aps\":{\"alert\":\"Hello!\"}}"
  describe "newCustomNotification" $ do
    it "creates a new custom notification" $ do
      let n = newCustomNotification "Custom!" (Custom "foo" 3)
      encode n `shouldBe` "{\"aps\":{\"alert\":\"Custom!\"},\"someone\":3,\"something\":\"foo\"}" 
  describe "newContentAvailableNotification" $ do
    it "creates a content available notification" $ do
      encode newContentAvailableNotification `shouldBe` "{\"aps\":{\"content-available\":1}}"
  describe "Alert" $ do
    it "serializes as an object when there's more than just a body" $ do
      let n = newNotification "Hello!" & apsL.alertL.titleL ?~ "Greeting" 
      encode n `shouldBe` "{\"aps\":{\"alert\":{\"body\":\"Hello!\",\"title\":\"Greeting\"}}}"
