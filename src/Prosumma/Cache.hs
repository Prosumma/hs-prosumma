{-# LANGUAGE RecordWildCards #-}

-- | Provides a thread-safe in-memory cache.
--
-- Operations which deal with the cache as a whole put "cache" at the end, e.g.,
-- @createCache@, @clearCache@. Operations which deal with individual cache entries
-- put "cache" at the beginning, e.g., @cacheGet@, @cachePut@.
--
-- Order of arguments closely follows those used for @Map@.
module Prosumma.Cache (
  cacheGet,
  cacheGetResult,
  cacheDelete,
  cachePut,
  clearCache,
  createCache,
  newCache,
  resultToMaybe,
  setCache,
  Cache,
  Result(..)
) where

import Data.Either.Extra
import RIO 
import RIO.Time

import qualified RIO.HashMap as HM

type FetchResult v = Either SomeException v
type Fetch k v = k -> IO v
type TTL = NominalDiffTime

data Entry v = Entry {
  entryWhen :: !UTCTime,
  entryValue :: !v
}

type Store k v = HashMap k (Entry v)

data Cache k v = Cache {
  cacheStore :: !(MVar (Store k v)),
  cacheTTL   :: !(Maybe TTL),
  cacheFetch :: !(Fetch k v)
}

data Result v = Cached !v | Fetched !Bool !(FetchResult v)

resultToMaybe :: Result v -> Maybe v
resultToMaybe (Cached v) = Just v
resultToMaybe (Fetched _ (Right v)) = Just v
resultToMaybe _ = Nothing

data NoException = NoException deriving (Show, Typeable)
instance Exception NoException

noException :: SomeException
noException = toException NoException

storePut :: (Hashable k, MonadIO m) => k -> FetchResult v -> Store k v -> m (Store k v)
storePut key (Left _) store = return $ HM.delete key store 
storePut key (Right value) store = do 
  entry <- liftIO $ Entry <$> getCurrentTime <*> pure value
  return $ HM.insert key entry store

storeGetResult :: (Hashable k, MonadUnliftIO m) => k -> Maybe TTL -> Fetch k v -> Store k v -> m (Result v, Store k v)
storeGetResult key ttl fetch store = case HM.lookup key store of
  Just Entry{..} -> do 
    isStale <- isStaleM entryWhen 
    if isStale
      then getFetchResult True store 
      else return (Cached entryValue, store)
  Nothing -> getFetchResult False store 
  where
    getFetchResult isStale store = do
      fetchResult <- liftIO $ catchAny (Right <$> fetch key) (return . Left) 
      newStore <- storePut key fetchResult store
      return (Fetched isStale fetchResult, newStore)
    isStaleM entryWhen = case ttl of
      Nothing -> return False
      Just ttl -> do 
        now <- liftIO getCurrentTime
        return $ diffUTCTime now entryWhen > ttl

newStore :: MonadIO m => HashMap k v -> m (Store k v)
newStore store = liftIO getCurrentTime >>= \now -> return $ HM.map (Entry now) store

cachePut :: (Hashable k, MonadIO m) => k -> Maybe v -> Cache k v -> m ()
cachePut key maybeValue Cache{..} = takeMVar cacheStore >>= storePut key (maybeToEither noException maybeValue) >>= putMVar cacheStore

cacheDelete :: (Hashable k, MonadIO m) => k -> Cache k v -> m ()
cacheDelete key = cachePut key Nothing

cacheGetResult :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m (Result v)
cacheGetResult key Cache{..} = do
  store <- takeMVar cacheStore
  (result, resultStore) <- storeGetResult key cacheTTL cacheFetch store
  putMVar cacheStore resultStore
  return result

cacheGet :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m (Maybe v)
cacheGet key cache = resultToMaybe <$> cacheGetResult key cache

createCache :: (Hashable k, MonadIO m) => Maybe Int -> Fetch k v -> m (Cache k v)
createCache ttl cacheFetch = do
  let cacheTTL = fromIntegral <$> ttl
  cacheStore <- newMVar mempty
  return Cache{..}

setCache :: MonadIO m => HashMap k v -> Cache k v -> m ()
setCache store Cache{..} = newStore store >>= void . swapMVar cacheStore 

newCache :: (Hashable k, MonadIO m) => Maybe Int -> Fetch k v -> HashMap k v -> m (Cache k v)
newCache ttl fetch store = do 
  cache <- createCache ttl fetch
  setCache store cache
  return cache

clearCache :: (Hashable k, MonadIO m) => Cache k v -> m ()
clearCache = setCache mempty