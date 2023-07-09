module Spec.Cache (testCache) where

import Prosumma.Cache
import RIO
import System.Random
import Test.Hspec

get :: Text -> IO (Maybe Int)
get "one" = return $ Just 1
get "two" = return $ Just 2
get "random" = Just <$> randomRIO (1, 100000)
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