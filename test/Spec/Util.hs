{-# LANGUAGE FunctionalDependencies, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Spec.Util (testUtil) where

import Control.Lens (each, _Just)
import Formatting ((%))
import Prosumma.Util
import RIO
import RIO.Map (fromList)
import Test.Hspec

import qualified Formatting as F
import qualified RIO.HashMap as HashMap
import qualified RIO.Set as Set

data Structure = Structure {
  strucText :: !Text,
  strucInt :: !Int
} deriving (Eq, Show)

makeProsummaLenses ''Structure

data Something = Something {
  x :: !Int,
  xs :: ![Maybe Int]
} deriving (Eq, Show)

makeLensesWith addL ''Something

testUtil :: Spec
testUtil = do
  describe "<->" $
    it "stitches together a pair" $
      2 <-> 3 `shouldBe` (2, 3)
  describe "<#>" $
    it "stitches together a pair into a list" $ do
      let expected = fromList [(2, 3), (4, 5)]
      let tested = fromList $ 2 <#> 3 <> 4 <#> 5
      expected `shouldBe` tested
  describe "<=>" $
    it "stitches together a pair into a Map" $ do
      let expected = fromList [(2, 3), (4, 5)]
      let tested = 2 <=> 3 <> 4 <=> 5
      expected `shouldBe` tested
  describe "makeProsummaLenses" $
    it "makes abbreviated lenses" $ do
      let structure = Structure "xyz" 3
      structure^.text `shouldBe` "xyz"
      structure^.int `shouldBe` 3
      (structure & int .~ 44) `shouldBe` Structure "xyz" 44
  describe "addL" $ do
    it "makes lenses with the suffix L" $ do
      let something = Something 2 [Just 4]
      something^.xL `shouldBe` 2
      something^..xsL.each._Just `shouldBe` [4]
  describe "<<$>>" $
    it "fmaps fmap" $ do
      let x = Just (Just 3)
      (*2) <<$>> x `shouldBe` Just (Just 6)
  describe "<<&>>" $
    it "fmaps fmap, but flipped" $ do
      let x = Just (Just 3)
      (x <<&>> (*7)) `shouldBe` Just (Just 21)
  describe "uformat" $
    it "formats a RIO Utf8Builder" $ do
      let u = uformat ("The value of " % F.text % " is " % F.int % ".") "three" 3
      utf8BuilderToText u `shouldBe` "The value of three is 3."
  describe "extractKeys" $ do
    it "extracts specific keys from a HashMap" $ do
      let hm :: HashMap Text Text = HashMap.fromList ["foo" <-> "bar", "bar" <-> "baz", "biz" <-> "bee"]
      let ex = extractKeys (Set.fromList ["foo", "bar"]) hm
      ex `shouldBe` HashMap.fromList ["foo" <-> "bar", "bar" <-> "baz"]