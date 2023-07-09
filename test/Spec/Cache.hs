module Spec.Cache (testCache) where

import Prosumma.Cache
import RIO
import System.Random
import Test.Hspec

import qualified RIO.Map as Map

get :: Text -> IO (Maybe Int)
get "one" = return $ Just 1
get "two" = return $ Just 2
get "random" = Just <$> randomRIO (1, 100000)
get "exception" = error "exception" 
get _ = return Nothing

testCache :: Spec
testCache = do
  describe "cacheGet with no TTL" $ do
    it "gets something if it can be got" $ do
      cache <- createCache Nothing get
      one <- cacheGet "one" cache
      one `shouldBe` Just 1
    it "gets nothing if it can't be got" $ do
      cache <- createCache Nothing get
      nada <- cacheGet "nada" cache
      nada `shouldBe` Nothing
    it "has no concept of a stale value" $ do
      cache <- createCache Nothing get
      firstOne <- cacheGet "random" cache
      secondOne <- cacheGet "random" cache
      firstOne `shouldBe` secondOne
    it "caches across threads" $ do
      cache <- createCache Nothing get
      thread1 <- async $ cacheGet "random" cache 
      thread2 <- async $ cacheGet "random" cache
      result <- waitBoth thread1 thread2
      let first = fst result
      let second = snd result
      first `shouldBe` second
    it "safely handles exceptions" $ do
      cache <- createCache Nothing get
      thread1 <- async $ cacheGet "exception" cache
      value <- cacheGet "one" cache
      threadValue <- catchAny (wait thread1) $ const (return (Just 0)) 
      (value, threadValue) `shouldBe` (Just 1, Just 0)
  describe "cacheGet with TTL" $ do
    it "gets something if it can be got" $ do
      cache <- createCache (Just 1) get
      firstOne <- cacheGet "random" cache
      secondOne <- cacheGet "random" cache
      firstOne `shouldBe` secondOne
    it "gets nothing if it can't be got" $ do
      cache <- createCache (Just 1) get
      nada <- cacheGet "nada" cache
      nada `shouldBe` Nothing
    it "refetches a stale value" $ do
      cache <- createCache (Just 1) get
      firstOne <- cacheGet "random" cache
      threadDelay 1100000
      secondOne <- cacheGet "random" cache
      firstOne `shouldNotBe` secondOne
    it "caches across threads" $ do
      cache <- createCache (Just 1) get
      thread1 <- async $ cacheGet "random" cache 
      thread2 <- async $ cacheGet "random" cache
      result <- waitBoth thread1 thread2
      let first = fst result
      let second = snd result
      first `shouldBe` second
    it "refetches a stale value across threads" $ do
      cache <- createCache (Just 1) get
      thread1 <- async $ cacheGet "random" cache
      thread2 <- async $ do
        threadDelay 1100000
        cacheGet "random" cache
      result <- waitBoth thread1 thread2
      let first = fst result
      let second = snd result
      first `shouldNotBe` second
    it "safely handles exceptions" $ do
      cache <- createCache (Just 1) get
      thread1 <- async $ cacheGet "exception" cache
      value <- cacheGet "one" cache
      threadValue <- catchAny (wait thread1) $ const (return (Just 0)) 
      (value, threadValue) `shouldBe` (Just 1, Just 0)
  describe "clearCache" $ do
    it "clears the cache" $ do
      cache <- createCache Nothing get
      first <- cacheGet "random" cache
      second <- cacheGet "random" cache
      clearCache cache
      third <- cacheGet "random" cache
      first `shouldBe` second
      first `shouldNotBe` third
  describe "cachePut" $ do
    it "puts a value directly into the cache" $ do
      cache <- createCache Nothing get
      cachePut "random" (Just 0) cache
      zero <- cacheGet "random" cache
      zero `shouldBe` Just 0
      cachePut "random" Nothing cache
      rand <- cacheGet "random" cache
      rand `shouldNotBe` Just 0
  describe "setCache" $ do
    it "sets all entries in the cache" $ do
      cache <- createCache (Just 1) get
      setCache (Map.singleton "random" 1) cache
      one <- cacheGet "random" cache
      one `shouldBe` Just 1
      threadDelay 1100000
      rand <- cacheGet "random" cache
      rand `shouldNotBe` Just 1
  describe "newCache" $ do
    it "creates a new Cache with preset values" $ do
      cache <- newCache Nothing get $ Map.singleton "random" 0 
      zero <- cacheGet "random" cache
      two <- cacheGet "two" cache
      (zero, two) `shouldBe` (Just 0, Just 2)
      clearCache cache
      rand <- cacheGet "random" cache
      rand `shouldNotBe` Just 0
