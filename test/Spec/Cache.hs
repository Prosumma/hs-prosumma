module Spec.Cache (testCache) where

import Prosumma.Cache
import RIO
import System.Random
import Test.Hspec

import qualified RIO.HashMap as HM

newtype MissingValueException = MissingValueException Text deriving (Show, Typeable)
instance Exception MissingValueException

get :: Text -> IO (Either SomeException (Maybe Int))
get "one" = return $ Right (Just 1)
get "random" = Right . Just <$> randomRIO (1, 100000)
get "exception" = error "exception"
get "nothing" = return $ Right Nothing
get key = return $ Left $ toException (MissingValueException key)

isCached :: Eq v => Maybe (Maybe v) -> Result v -> Bool
isCached Nothing (Cached _) = True
isCached (Just lhs) (Cached rhs) = lhs == rhs
isCached _ _ = False

isFetched :: Eq v => Maybe Bool -> Maybe (Either SomeException (Maybe v)) -> Result v -> Bool
isFetched Nothing Nothing (Fetched _ _) = False
isFetched Nothing (Just lhs) (Fetched _ rhs) = lhs == rhs 
isFetched (Just lhsStale) Nothing (Fetched rhsStale _) = lhsStale == rhsStale
isFetched (Just lhsStale) (Just lhs) (Fetched rhsStale rhs) = lhs == rhs && lhsStale == rhsStale
isFetched _ _ _ = False

testCache :: Spec
testCache = do
  describe "cacheGetIntent" $ do
    it "shows intent to fetch a missing value" $ do
      cache <- createCache Nothing get
      result <- cacheGetIntent "one" cache
      result `shouldSatisfy` isFetched (Just False) (Just $ Right Nothing) 