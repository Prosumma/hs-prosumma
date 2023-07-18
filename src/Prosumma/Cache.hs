{-# LANGUAGE RecordWildCards #-}

-- | Provides a thread-safe in-memory cache.
--
-- Operations which deal with the cache as a whole put "cache" at the end, e.g.,
-- @createCache@, @clearCache@. Operations which deal with individual cache entries
-- put "cache" at the beginning, e.g., @cacheGet@, @cachePut@.
--
-- Order of arguments closely follows those used for @Map@.
module Prosumma.Cache (
  cacheDelete,
  cacheDeletes,
  cacheGet,
  cacheGets,
  cacheGetResult,
  cacheGetResults,
  cacheInsert,
  cacheInserts,
  cachePut,
  cachePuts,
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

data Result v = Cached !v | Fetched !Bool !(FetchResult v) deriving Show

data NoException = NoException deriving (Show, Typeable)
instance Exception NoException

noException :: SomeException
noException = toException NoException

resultToMaybe :: Result v -> Maybe v
resultToMaybe (Cached v) = Just v
resultToMaybe (Fetched _ (Right v)) = Just v
resultToMaybe _ = Nothing

maybeToFetchResult :: Maybe v -> FetchResult v
maybeToFetchResult (Just v) = Right v
maybeToFetchResult Nothing = Left noException

put :: Hashable k => UTCTime -> k -> FetchResult v -> Store k v -> Store k v
put _ key (Left _) store = HM.delete key store
put now key (Right value) store = HM.insert key (Entry now value) store

storePut :: (Hashable k, MonadIO m) => k -> FetchResult v -> Store k v -> m (Store k v)
storePut key value store = do
  now <- liftIO getCurrentTime
  return $ put now key value store

storePuts :: (Hashable k, MonadIO m) => HashMap k (FetchResult v) -> Store k v -> m (Store k v)
storePuts newEntries store = do
  now <- liftIO getCurrentTime
  return $ HM.foldrWithKey (put now) store newEntries

-- | See the 'cacheGetResult' function for a discussion.
storeGetResult
  :: (Hashable k, MonadUnliftIO m)
  => k -> Maybe TTL -> Fetch k v -> Store k v -> m (Result v, Store k v)
storeGetResult key ttl fetch store = case (HM.lookup key store, ttl) of
  (Just entry, Just ttl) -> handleStale entry ttl store -- Check staleness
  (Just Entry{..}, Nothing) -> return (Cached entryValue, store)  -- Entry was found, no TTL specified
  _ -> getFetchResult False store -- Entry must be fetched
  where
    handleStale Entry{..} ttl store = do
      now <- liftIO getCurrentTime
      if diffUTCTime now entryWhen <= ttl
        then return (Cached entryValue, store) -- Entry was found and is not stale
        else getFetchResult True store -- Entry was found but is stale
    getFetchResult isStale store = do
      fetchResult <- liftIO $ catchAny (Right <$> fetch key) (return . Left)
      newStore <- storePut key fetchResult store
      return (Fetched isStale fetchResult, newStore)

storeGetResults
  :: (Hashable k, MonadUnliftIO m, Foldable f)
  => f k -> Maybe TTL -> Fetch k v -> Store k v -> m (HashMap k (Result v), Store k v)
storeGetResults keys ttl fetch store = foldM get (mempty, store) keys
  where
    get (results, store) key = do
      (result, nextStore) <- storeGetResult key ttl fetch store
      return (HM.insert key result results, nextStore)

newStore :: MonadIO m => HashMap k v -> m (Store k v)
newStore store = liftIO getCurrentTime >>= \now -> return $ HM.map (Entry now) store

cachePut :: (Hashable k, MonadIO m) => k -> Maybe v -> Cache k v -> m ()
cachePut key maybeValue Cache{..} = takeMVar cacheStore
  >>= storePut key (maybeToFetchResult maybeValue)
  >>= putMVar cacheStore

cachePuts :: (Hashable k, MonadIO m) => HashMap k (Maybe v) -> Cache k v -> m ()
cachePuts newEntries Cache{..} = takeMVar cacheStore
  >>= storePuts (HM.map maybeToFetchResult newEntries)
  >>= putMVar cacheStore

cacheDelete :: (Hashable k, MonadIO m) => k -> Cache k v -> m ()
cacheDelete key = cachePut key Nothing

cacheDeletes :: (Hashable k, MonadIO m, Foldable f) => f k -> Cache k v -> m ()
cacheDeletes = cachePuts . foldr (`HM.insert` Nothing) mempty

cacheInsert :: (Hashable k, MonadIO m) => k -> v -> Cache k v -> m ()
cacheInsert key = cachePut key . Just

cacheInserts :: (Hashable k, MonadIO m) => HashMap k v -> Cache k v -> m ()
cacheInserts newEntries = cachePuts $ HM.map Just newEntries

-- | Attempts to get an entry and reports the result in the 'Result' type.
-- This function is thread-safe.
--
-- In most cases, callers will not care what the @Result@ is. They just want the
-- value. In that case, use 'cacheGet'. 
cacheGetResult :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m (Result v)
cacheGetResult key Cache{..} = do
  store <- takeMVar cacheStore
  (result, resultStore) <- storeGetResult key cacheTTL cacheFetch store
  putMVar cacheStore resultStore
  return result

-- | Same as 'cacheGetResult' but gets more than one key.
--
-- When attempting to get multiple keys, this method is more efficient than calling @cacheGetResult@ multiple times.
cacheGetResults :: (Hashable k, MonadUnliftIO m, Foldable f) => f k -> Cache k v -> m (HashMap k (Result v))
cacheGetResults keys Cache{..} = do
  store <- takeMVar cacheStore
  (result, resultStore) <- storeGetResults keys cacheTTL cacheFetch store
  putMVar cacheStore resultStore
  return result

-- | Simplified version of 'cacheGet 
cacheGet :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m (Maybe v)
cacheGet key cache = resultToMaybe <$> cacheGetResult key cache

cacheGets :: (Hashable k, MonadUnliftIO m, Foldable f) => f k -> Cache k v -> m (HashMap k (Maybe v))
cacheGets keys cache = HM.map resultToMaybe <$> cacheGetResults keys cache

createCache :: (Hashable k, MonadIO m) => Maybe Int -> Fetch k v -> m (Cache k v)
createCache ttl cacheFetch = newCache ttl cacheFetch mempty

setCache :: MonadIO m => HashMap k v -> Cache k v -> m ()
setCache store Cache{..} = newStore store >>= void . swapMVar cacheStore

newCache :: MonadIO m => Maybe Int -> Fetch k v -> HashMap k v -> m (Cache k v)
newCache ttl cacheFetch store = do
  let cacheTTL = fromIntegral <$> ttl
  cacheStore <- newStore store >>= newMVar
  return Cache{..}

clearCache :: (Hashable k, MonadIO m) => Cache k v -> m ()
clearCache = setCache mempty