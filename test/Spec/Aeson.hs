module Spec.Aeson (testAeson) where

import Data.Aeson
import Data.Default
import Prosumma.Aeson
import RIO
import Test.Hspec

data Foo = Foo {
  fooBar :: !(Maybe Text),
  fooData :: !(Maybe Foo),
  fooWhat :: !Bool,
  fooFoo :: ![Foo]
}

instance Default Foo where
  def = Foo Nothing Nothing False mempty

instance ToJSON Foo where
  toJSON Foo{..} = stripJSON (ofAll InBoth) $ object [
      "bar"  .= fooBar,
      "data" .= fooData,
      "what" .= fooWhat,
      "foos" .= fooFoo
    ]

testAeson :: Spec
testAeson = do
  describe "stripJSON stripAll" $ do
    it "strips empty nodes (null, [], {}, empty strings, and false) recursively from objects and arrays" $ do
      let nested = def { fooBar = Just "  " }
      let foo = Foo (Just "Bar") (Just def) True [nested, def] 
      let j = encode foo
      j `shouldBe` "{\"bar\":\"Bar\",\"what\":true}"
