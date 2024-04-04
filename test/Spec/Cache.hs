{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

module Spec.Cache (testCache) where

import Prosumma.Cache
import RIO
import RIO.List (sort)
import System.Random
import Test.Hspec

import qualified RIO.HashMap as HashMap
import qualified RIO.Set as Set

data MissingValueException = MissingValueException deriving Show
instance Exception MissingValueException

missingValueException :: Proxy MissingValueException
missingValueException = Proxy

isException :: forall e. Exception e => Proxy e -> SomeException -> Bool
isException _ ex = isJust (fromException ex :: Maybe e)

get :: Text -> IO Int
get "one" = return 1
get "two" = return 2
get "three" = return 3
get "four" = return 4
get "random" = randomRIO (1, 100000)
get _ = throwIO MissingValueException

resultShouldBe :: (Eq v, Show v) => Result v -> (v, Outcome) -> Expectation
resultShouldBe actual expected = actual `shouldSatisfy` either (const False) (== expected)

resultShouldThrow :: (Show v, Exception e) => Result v -> Proxy e -> Expectation
resultShouldThrow actual proxy = actual `shouldSatisfy` either (isException proxy) (const False) 

testCache :: Spec
testCache = do
  describe "cacheGetResult" $ do
    it "fetches an existing value from the cache" $ do
      cache <- newCache return get (HashMap.singleton "one" 1) 
      result <- cacheGetResult "one" cache
      result `resultShouldBe` (1, Cached)
    it "fetches a value when it is not present in the cache" $ do
      cache <- createCache return get
      result <- cacheGetResult "two" cache
      result `resultShouldBe` (2, Fetched)
    it "does not cache if an exception occurs" $ do
      cache <- createCache return get
      result <- cacheGetResult "seventy" cache
      result `resultShouldThrow` missingValueException
    it "works across threads" $ do
      cache <- createCache return get
      let getRandom = cacheGet "random" cache
      let threadCount = [1..100] :: [Int]
      values <- forConcurrently threadCount $ const getRandom
      values `shouldSatisfy` allEqual
  describe "cacheGets" $ do
    it "gets multiple items from the cache, skipping entries which fail" $ do
      cache <- createCache return get
      results <- cacheGets (Set.fromList ["one", "two", "eleven"]) cache 
      sort results `shouldBe` [1, 2]
  describe "withTTLBy" $ do
    it "reaps an item if it has expired" $ do
      cache <- createCache (withTTLBy accessed 0.1) get
      result1 <- cacheGetResult "one" cache
      result1 `resultShouldBe` (1, Fetched)
      result2 <- cacheGetResult "one" cache
      result2 `resultShouldBe` (1, Cached)
      threadDelay 150000
      result3 <- cacheGetResult "one" cache
      result3 `resultShouldBe` (1, Fetched)
  describe "cachePut" $ do
    it "puts an item manually into the cache" $ do
      cache <- createCache return get
      cachePut "one" (Just 1) cache
      result <- cacheGetResult "one" cache
      result `resultShouldBe` (1, Cached)
  describe "withSizeLimitBy" $ do
    it "reaps items if the cache grows above a certain size" $ do
      cache <- createCache (withSizeLimitBy accessed 2) get 
      void $ cacheGet "one" cache
      void $ cacheGet "two" cache
      void $ cacheGet "three" cache
      result <- cacheGetResult "one" cache
      result `resultShouldBe` (1, Fetched)
  describe "cacheDelete" $ do
    it "removes an item from the cache so it can be refetched" $ do
      cache <- newCache return get $ HashMap.singleton "one" 1
      result1 <- cacheGetResult "one" cache
      result1 `resultShouldBe` (1, Cached)
      cacheDelete "one" cache
      result2 <- cacheGetResult "one" cache
      result2 `resultShouldBe` (1, Fetched)

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (==x) xs
