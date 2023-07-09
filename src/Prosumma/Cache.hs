{-# LANGUAGE RecordWildCards #-}

-- | Provides a thread-safe in-memory cache.
--
-- Operations which deal with the cache as a whole put "cache" at the end, e.g.,
-- @createCache@, @clearCache@. Operations which deal with individual cache entries
-- put "cache" at the beginning, e.g., @cacheGet@, @cachePut@.
--
-- Order of arguments closely follows those used for @Map@.
module Prosumma.Cache (
  clearCache,
  createCache,
  cacheGet,
  cachePut,
  newCache,
  setCache,
  Cache
) where

import Data.Time
import RIO 

import qualified RIO.Map as Map

data Entry v = Entry {
  entryTime :: !UTCTime,
  entryValue :: !v
}

newEntry :: MonadIO m => v -> m (Entry v)
newEntry value = Entry <$> liftIO getCurrentTime <*> pure value 

isEntryStale :: MonadIO m => Maybe Int -> Entry v -> m Bool
isEntryStale Nothing _ = return False
isEntryStale (Just seconds) Entry{..} = do
  now <- liftIO getCurrentTime
  return $ round (diffUTCTime now entryTime) >= seconds

-- | Provides a simple, thread-safe cache in memory.
--
-- This should not be used to store large amounts of data.
data Cache k v = Cache {
  cacheStore :: !(MVar (Map k (Entry v))),
  cacheTTL   :: !(Maybe Int),
  cacheFetch :: !(k -> IO (Maybe v))
}

createCache :: MonadIO m => Maybe Int -> (k -> IO (Maybe v)) -> m (Cache k v)
createCache cacheTTL cacheFetch = do
  cacheStore <- newMVar Map.empty
  return Cache{..}

-- | Creates a cache and also initializes it.
newCache :: MonadIO m => Maybe Int -> (k -> IO (Maybe v)) -> Map k v -> m (Cache k v)
newCache ttl fetch store = do
  cache <- createCache ttl fetch 
  setCache store cache
  return cache

clearCache :: MonadIO m => Cache k v -> m ()
clearCache Cache{..} = void $ swapMVar cacheStore Map.empty 

-- | Sets the entire contents of the @Cache@ to the new values.
setCache :: MonadIO m => Map k v -> Cache k v -> m ()
setCache newStore Cache{..} = do
  now <- liftIO getCurrentTime
  void $ swapMVar cacheStore $ Map.map (Entry now) newStore

cacheGetEntry :: (Ord k, MonadUnliftIO m) => k -> Cache k v -> m (Maybe (Entry v))
cacheGetEntry key Cache{..} = do
  store <- takeMVar cacheStore
  case Map.lookup key store of
    Just entry -> do
      isStale <- isEntryStale cacheTTL entry
      if isStale
        then cacheFetchEntry store 
        else do
          putMVar cacheStore store
          return $ Just entry
    Nothing -> cacheFetchEntry store 
  where
    cacheFetchEntry store = do
      maybeValue <- catchAny (liftIO $ cacheFetch key) $ handleException store
      case maybeValue of
        Nothing -> do
          putMVar cacheStore store
          return Nothing
        Just value -> do 
          ne <- newEntry value
          putMVar cacheStore $ Map.insert key ne store 
          return $ Just ne
    handleException store e = do
      putMVar cacheStore store
      throwIO e

-- | Gets an entry from the given @Cache@. This operation is thread-safe.
--
-- If there's a cache miss because the entry is not present or is stale,
-- the @cacheFetch@ function passed to @createCache@ is used to attempt
-- to fetch the value. 
cacheGet :: (Ord k, MonadUnliftIO m) => k -> Cache k v -> m (Maybe v)
cacheGet key cache = (entryValue <$>) <$> cacheGetEntry key cache 

-- | Puts a value directly into the cache, or clears it if the value passed is @Nothing@.
cachePut :: (Ord k, MonadIO m) => k -> Maybe v -> Cache k v -> m () 
cachePut key (Just value) Cache{..} = do
  ne <- newEntry value
  store <- takeMVar cacheStore
  putMVar cacheStore $ Map.insert key ne store
cachePut key Nothing Cache{..} = do 
  store <- takeMVar cacheStore
  putMVar cacheStore $ Map.delete key store
