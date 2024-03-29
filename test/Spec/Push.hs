module Spec.Push (testPush) where

import Control.Lens ((?~))
import Data.Aeson
import Prosumma.Push
import RIO
import Test.Hspec

import qualified Data.Aeson.KeyMap as KM

testPush :: Spec
testPush = do 
  describe "Push" $ do
    it "serializes properly" $ do
      let push = newPush (Message "Hello!")
            & badge ?~ 5
            & customData .~ KM.fromList ["x" .= (3 :: Int)]
      let j = encode push
      j `shouldBe` "{\"badge\":5,\"body\":\"Hello!\",\"data\":{\"x\":3}}"
    it "deserializes properly" $ do
      let j = "{\"badge\":5,\"body\":\"Hello!\",\"data\":{\"x\":3}}"
      let push :: Maybe Push = decode j
      let expected = newPush (Message "Hello!")
            & badge ?~ 5
            & customData .~ KM.fromList ["x" .= (3 :: Int)]
      push `shouldBe` Just expected 
