{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Spec.AWS.DynamoDB (testDynamoDB) where

import Amazonka
import Amazonka.DynamoDB
import Prosumma.AWS.DynamoDB
import RIO
import RIO.Partial (fromJust)
import Test.Hspec

import qualified RIO.HashMap as HM
import qualified RIO.Time as Time

newtype Sub = Sub Int deriving (Eq, Show)

instance FromAttributeValue Sub where
  fromAttributeValue = readAttributeItem $ \read -> Sub <$> read "n"

instance ToAttributeValue Sub where
  toAttributeValue (Sub n) = writeAttributeItem ["n" =: n]

data Watusi = Watusi {
  watusiFoo :: !Text,
  watusiBar :: !Int,
  watusiBaz :: !(Maybe Text),
  watusiBiz :: !(Maybe [Text]),
  watusiSub :: !Sub,
  watusiTim :: !UTCTime
} deriving (Eq, Show)

instance FromTableItem Watusi where
  fromTableItem = readTableItem $ \read -> Watusi <$> read "foo" <*> read "bar" <*> read "baz" <*> read "biz" <*> read "sub" <*> read "tim"

instance ToTableItem Watusi where
  toTableItem Watusi{..} = writeTableItem [
      "foo" =: watusiFoo,
      "bar" =: watusiBar,
      "baz" =: watusiBaz,
      "biz" =: watusiBiz,
      "sub" =: watusiSub,
      "tim" =: watusiTim
    ]

getCurrentTimeRoundTripped :: MonadIO m => m UTCTime
getCurrentTimeRoundTripped = fromJust . Time.parseTimeM True Time.defaultTimeLocale utcTimeFormat . Time.formatTime Time.defaultTimeLocale utcTimeFormat <$> Time.getCurrentTime

testDynamoDB :: Spec
testDynamoDB = do
  describe "readTableItem" $ do
    it "reads a table item" $ do
      time <- getCurrentTimeRoundTripped 
      let watusi = Watusi "cool" 35 (Just "baz") (Just ["a", "b"]) (Sub 3) time
      let item = toTableItem watusi
      let result = fromTableItem item
      result `shouldBe` Right watusi
    it "gives an error if a key is not found" $ do
      let item = HM.empty
      let result :: Either ValueError Watusi = fromTableItem item
      result `shouldBe` Left (ValueMissing (Just "foo"))
    it "gives an error if a key is found but is not the correct type" $ do
      let item = HM.fromList [("foo", BOOL False)]
      let result :: Either ValueError Watusi = fromTableItem item
      result `shouldBe` Left (ValueInvalid (Just "foo") (BOOL False) Nothing)