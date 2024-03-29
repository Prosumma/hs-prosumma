module Spec.Cache (testCache) where

import Prosumma.Cache
import RIO
import RIO.HashMap.Partial ((!))
import System.Random
import Test.Hspec

import qualified RIO.HashMap as HM

newtype MissingValueException = MissingValueException Text deriving (Show, Typeable)
instance Exception MissingValueException

get :: Text -> IO (Maybe Int)  
get "one" = return $ Just 1 
get "random" = Just <$> randomRIO (1, 100000)
get "exception" = error "exception"
get _ = return Nothing 

isFetched :: Eq v => Bool -> v -> Result v -> Bool
isFetched staleExpected a (Fetched staleActual (Right b)) = staleExpected == staleActual && a == b 
isFetched _ _ (Fetched _ (Left _)) = False 
isFetched _ _ (Cached _) = False

isFetchError :: Result v -> Bool
isFetchError (Fetched _ (Left _)) = True 
isFetchError _ = False

testCache :: Spec
testCache = do 
  describe "cacheGetResult" $ do
    it "fetches an existing value from the cache" $ do
      cache <- newCache Nothing get $ HM.singleton "random" (Just 2)
      result <- cacheGetResult "random" cache
      result `shouldBe` Cached (Just 2) 
    it "refetches a stale value from the cache" $ do
      cache <- newCache (Just 0) get $ HM.singleton "random" (Just 0)
      result <- cacheGetResult "random" cache
      let value = join $ resultToMaybe result
      result `shouldSatisfy` isFetched True value 
    it "fetches an absent value from the cache" $ do
      cache <- createCache Nothing get
      result <- cacheGetResult "one" cache
      result `shouldSatisfy` isFetched False (Just 1)
    it "fetches an unexpired value from the cache" $ do
      cache <- newCache (Just 5) get $ HM.singleton "one" (Just 1)
      result <- cacheGetResult "one" cache
      result `shouldBe` Cached (Just 1)
    it "does not cache if an exception occurs" $ do
      cache <- createCache Nothing get
      result <- cacheGetResult "exception" cache
      result `shouldSatisfy` isFetchError
    it "works across threads" $ do
      cache <- createCache Nothing get
      let getRandom = join <$> cacheGet "random" cache
      let threadCount = [1..100] :: [Int]
      values <- forConcurrently threadCount $ const getRandom
      values `shouldSatisfy` allEqual
  describe "cacheGetAll" $ do
    it "gets all entries" $ do
      let entries = HM.fromList [("foo", Just 999), ("bar", Just 666)]
      cache <- newCache Nothing get entries 
      results <- HM.map join <$> cacheGetAll cache
      results `shouldBe` entries 
  describe "cacheGets" $ do
    it "gets many results as a HashMap" $ do
      cache <- newCache Nothing get $ HM.singleton "two" (Just 2)
      results <- HM.map join <$> cacheGets ["one", "two"] cache
      results `shouldBe` HM.fromList [("one", Just 1), ("two", Just 2)]
    it "handles any Foldable" $ do
      cache <- newCache Nothing get $ HM.singleton "two" (Just 2)
      results <- cacheGets Nothing cache
      results `shouldBe` mempty
  describe "cachePut" $ do
    it "puts a value into the cache" $ do
      cache <- createCache Nothing get
      cachePut "random" (Just (Just 0)) cache
      value <- join <$> cacheGet "random" cache
      value `shouldBe` Just 0 
  describe "cachePuts" $ do
    it "puts multiple values into the cache" $ do
      cache <- newCache Nothing get $ HM.fromList [("one", Just 5), ("random", Just 0)]
      let newEntries = HM.fromList [("one", Nothing), ("random", Nothing), ("x", Just (Just 9))]
      cachePuts newEntries cache
      values <- HM.map join <$> cacheGets ["one", "random", "x"] cache
      values ! "random" `shouldNotBe` Just 0
      values ! "one" `shouldBe` Just 1
      values ! "x" `shouldBe` Just 9
  describe "cacheDelete" $ do
    it "removes a value from the cache" $ do
      cache <- newCache Nothing get $ HM.singleton "ninety" (Just 90)
      cacheDelete "ninety" cache
      value <- join <$> cacheGet "ninety" cache
      value `shouldBe` Nothing
  describe "cacheDeletes" $ do
    it "removes multiple values from the cache" $ do
      cache <- newCache Nothing get $ HM.fromList [("foo", Just 999), ("bar", Just 666)]
      cacheDeletes ["foo", "bar"] cache
      size <- sizeCache cache
      size `shouldBe` 0
  describe "cacheInsert" $ do
    it "inserts a value into the cache" $ do
      cache <- createCache Nothing get
      cacheInsert "foo" (Just 999) cache
      foo <- join <$> cacheGet "foo" cache
      foo `shouldBe` Just 999
  describe "cacheInserts" $ do
    it "inserts multiple values into the cache" $ do
      let newValues = HM.fromList [("foo", Just 999), ("bar", Just 666)]
      cache <- createCache Nothing get
      cacheInserts newValues cache
      values <- HM.map join <$> cacheGets ["foo", "bar"] cache
      values `shouldBe` newValues
  describe "setCache" $ do
    it "replaces the cache with new values" $ do
      cache <- createCache Nothing get
      let newStore = HM.singleton "twelve" (Just 12)
      setCache newStore cache
      value <- join <$> cacheGet "twelve" cache
      value `shouldBe` Just 12
  describe "clearCache" $ do
    it "clears the cache" $ do
      cache <- newCache Nothing get $ HM.singleton "random" (Just 0)
      clearCache cache
      value <- join <$> cacheGet "random" cache
      value `shouldNotBe` Just 0

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (==x) xs