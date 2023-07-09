module Spec.Cache (testCache) where

import Prosumma.Cache
import RIO
import System.Random
import Test.Hspec

import qualified RIO.Map as Map

get :: Text -> IO (Maybe Int)
get "random" = Just <$> randomRIO (1, 100000)
get "exception" = error "exception" 
get _ = return Nothing

testCache :: Spec
testCache = do
  describe "cacheGetResult" $ do
    it "fetches a value on cache miss" $ do
      cache <- createCache Nothing get
      result <- cacheGetResult "random" cache
      result `shouldBe` Fetched False (resultGet result)
    it "fulfills from the cache when a value is present" $ do
      cache <- newCache Nothing get $ Map.singleton "random" 0 
      result <- cacheGetResult "random" cache
      result `shouldBe` Cached 0
    it "fetches a value when a cached value is stale" $ do
      cache <- newCache (Just 0) get $ Map.singleton "random" 0
      result <- cacheGetResult "random" cache
      result `shouldBe` Fetched True (resultGet result)
  describe "cacheGet (and thus cacheGetResult)" $ do
    it "is thread safe" $ do
      cache <- createCache Nothing get
      thread1 <- async $ cacheGet "random" cache
      value2 <- cacheGet "random" cache
      value1 <- wait thread1
      value1 `shouldBe` value2
    it "handles exceptions properly by releasing the MVar" $ do
      cache <- createCache Nothing get
      thread1 <- async $ cacheGet "exception" cache
      wait thread1 `shouldThrow` errorCall "exception" 
      -- If the Cache fails to release the MVar, this will hang.
      value <- cacheGet "random" cache
      value `shouldNotBe` Nothing
  describe "newCache" $ do
    it "creates a cache with initial defaults" $ do
      cache <- newCache (Just 1) get $ Map.singleton "random" 0
      value1 <- cacheGet "random" cache
      value1 `shouldBe` Just 0
      threadDelay 1100000
      value2 <- cacheGet "random" cache
      value1 `shouldNotBe` value2
  describe "clearCache" $ do
    it "clears the cache" $ do
      cache <- createCache Nothing get
      value1 <- cacheGet "random" cache
      clearCache cache
      value2 <- cacheGet "random" cache
      value1 `shouldNotBe` value2
  describe "cachePut" $ do
    it "sets an individual value in the cache" $ do
      cache <- createCache Nothing get
      cachePut "random" (Just 0) cache
      value1 <- cacheGet "random" cache
      value1 `shouldBe` Just 0

