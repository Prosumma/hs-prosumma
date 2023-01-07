{-# LANGUAGE FunctionalDependencies, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Spec.Util (testUtil) where

import Prosumma.Util
import RIO
import RIO.Map (fromList)
import Test.Hspec

data Structure = Structure {
  strucText :: !Text,
  strucInt :: !Int
} deriving (Eq, Show)

makeProsummaLenses ''Structure

testUtil :: Spec
testUtil = do 
  describe "firstJust and firstJusts" $
    it "gets the first Just" $
      (Nothing ?? Just 2) `shouldBe` Just 2
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