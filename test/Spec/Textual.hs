module Spec.Textual (
  testTextual
) where

import Data.Aeson
import Data.Attoparsec.Text
import Data.String.Conversions
import Prosumma.Textual
import RIO
import Test.Hspec

import qualified RIO.Text as Text

newtype Foo = Foo Text deriving (Eq, Show)

instance FromText Foo where
  fromText = parseTextual $ do 
    let match = satisfy $ inClass "[a-z]"
    prefix <- count 2 match
    suffix <- fromMaybe mempty <$> optional (count 1 match) 
    endOfInput
    return $ Foo $ Text.pack $ prefix <> suffix

instance ToText Foo where
  toText (Foo foo) = foo

instance IsString Foo where
  fromString = fromStringTextual "Foo"

instance FromJSON Foo where
  parseJSON = parseJSONTextual "Foo"

testTextual :: Spec
testTextual = do
  describe "fromText with ifMatchTextual" $ do
    it "succeeds if the given text matches" $ do
      let foo = fromText "foo" :: Maybe Foo
      foo `shouldBe` Just (Foo "foo")
    it "fails if the given text does not match" $ do
      let foo = fromText "foobar" :: Maybe Foo
      foo `shouldBe` Nothing
  describe "toText" $
    it "always succeeds" $
      toText (Foo "bar") `shouldBe` "bar"
  describe "fromStringTextual" $
    it "succeeds" $ do
      let foo = "foo" :: Foo
      toText foo `shouldBe` "foo"
  describe "parseJSONTextual" $ do
    it "succeeds if the value is valid" $ do
      let json = "\"foo\"" :: ByteString
      let foo = decode (convertString json) :: Maybe Foo
      foo `shouldBe` Just (Foo "foo")
    it "fails if the value is invalid" $ do
      let json = "\"bing23\"" :: ByteString
      let foo = decode (convertString json) :: Maybe Foo
      foo `shouldBe` Nothing