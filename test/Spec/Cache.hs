{-# LANGUAGE NumericUnderscores #-}

module Spec.Cache (
  testCache
) where

import Prosumma.Cache
import RIO
import System.Random
import Test.Hspec

import qualified RIO.HashMap as HashMap

data MissingValueException = MissingValueException deriving Show
instance Exception MissingValueException

missingValueException :: Selector MissingValueException
missingValueException = const True

get :: Text -> IO Int
get "one" = return 1
get "two" = return 2
get "three" = return 3
get "four" = return 4
get "random" = randomRIO (1, 100000)
get _ = throwIO MissingValueException

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (==x) xs

testCache :: Spec
testCache = do
  describe "cacheGetResult" $ do
    it "fetches an existing value from the cache" $ do
      cache <- newCache get (HashMap.singleton "one" 1)
      result <- cacheGetResult "one" cache
      result `shouldBe` (1, Cached)
    it "fetches a value when it is not present in the cache" $ do
      cache <- newCache get mempty
      result <- cacheGetResult "one" cache
      result `shouldBe` (1, Fetched)
    it "does not cache if an exception occurs" $ do
      cache <- newCache get mempty
      cacheGetResult "seventy" cache `shouldThrow` missingValueException
  describe "cacheGet" $ do
    it "works across threads" $ do
      cache <- newCache get mempty
      let getRandom = cacheGet "random" cache
      let threadCount = [1..100] :: [Int]
      values <- forConcurrently threadCount $ const getRandom
      values `shouldSatisfy` allEqual
  describe "cachePut" $ do
    it "puts an item manually into the cache" $ do
      cache <- newCache get mempty
      cachePut "one" 1 cache
      result <- cacheGetResult "one" cache
      result `shouldBe` (1, Cached)
  describe "cacheDelete" $ do
    it "manually removes an item from the cache so it can be refetched" $ do
      cache <- newCache get (HashMap.singleton "one" 1)
      result1 <- cacheGetResult "one" cache
      result1 `shouldBe` (1, Cached)
      cacheDelete "one" cache
      result2 <- cacheGetResult "one" cache
      result2 `shouldBe` (1, Fetched)
  describe "withSizeLimitBy" $ do
    it "reaps items if the cache grows above a certain size" $ do
      cache <- newCache get mempty
      void $ cacheGet "one" cache
      void $ cacheGet "two" cache
      void $ cacheGet "three" cache
      flip finally (setReap cache 0 Nothing) $ do
        setReap cache 50_000 (Just $ withSizeLimitBy accessed 2)
        threadDelay 60_000
        result <- cacheGetResult "one" cache
        result `shouldBe` (1, Fetched)
  describe "withTTLBy" $ do
    it "reaps an item if it has expired" $ do
      cache <- newCache get mempty
      flip finally (setReap cache 0 Nothing) $ do
        setReap cache 50_000 (Just $ withTTLBy accessed 0.1)
        result1 <- cacheGetResult "one" cache
        result1 `shouldBe` (1, Fetched)
        result2 <- cacheGetResult "one" cache
        result2 `shouldBe` (1, Cached)
        threadDelay 150_000
        result3 <- cacheGetResult "one" cache
        result3 `shouldBe` (1, Fetched)
