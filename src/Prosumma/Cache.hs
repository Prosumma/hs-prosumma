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
  cacheGetStatus,
  cachePut,
  clearCache,
  createCache,
  newCache,
  resultGet,
  setCache,
  Cache,
  Result(..)
) where

import RIO 
import RIO.Time

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

-- | Gives detailed information about the result of a cache retrieval.
--
-- @Cache v@ indicates that the result was retrieved directly from the cache.
-- @Fetched True (Maybe v)@ indicates that a value was in the cache but it had
-- to be refetched because it had expired.
-- @Fetched False (Maybe v)@ indicates that a value was not in the cache and
-- had to be fetched.
--
-- This is chiefly important in unit tests.
data Result v = Cached v | Fetched Bool (Maybe v) deriving (Eq, Ord, Show)

resultGet :: Result v -> Maybe v
resultGet (Cached v) = Just v
resultGet (Fetched _ (Just v)) = Just v
resultGet (Fetched _ Nothing) = Nothing

storeGetStatus :: (Ord k, MonadIO m) => k -> Maybe Int -> Map k (Entry v) -> m (Result v)
storeGetStatus key ttl store = do
  case Map.lookup key store of
    Just entry -> do
      isStale <- isEntryStale ttl entry
      return $ if isStale then Fetched True Nothing else Cached (entryValue entry)
    Nothing -> return $ Fetched False Nothing  

-- | Gets the status of an entry using the @Result@ type. 
--
-- @cacheGetStatus@ never fetches a value. It simply returns an indication
-- of what the @Cache@ would have to do given the status of the entry.
--
-- - @Cached v@ - The value is cached and not stale.
-- - @Fetched True Nothing@ - The value is cached but stale and would have to be refetched.
-- - @Fetched False Nothing@ - The value is not present in the cache.
--
-- The second parameter of @Fetched@ will always be @Nothing@.
cacheGetStatus :: (Ord k, MonadIO m) => k -> Cache k v -> m (Result v)
cacheGetStatus key Cache{..} = readMVar cacheStore >>= storeGetStatus key cacheTTL 

-- | Gets the @Result@ of a cache retrieval. This operation is thread-safe.
cacheGetResult :: (Ord k, MonadUnliftIO m) => k -> Cache k v -> m (Result v)
cacheGetResult key Cache{..} = do
  store <- takeMVar cacheStore
  status <- storeGetStatus key cacheTTL store
  case status of
    Cached v -> putMVar cacheStore store >> return (Cached v) 
    Fetched isStale _ -> Fetched isStale <$> cacheFetchValue store 
  where
    cacheFetchValue store = do
      maybeValue <- catchAny (liftIO $ cacheFetch key) $ handleException store
      case maybeValue of
        Just value -> do
          ne <- newEntry value
          putMVar cacheStore (Map.insert key ne store)
          return $ Just (entryValue ne)
        Nothing -> do
          putMVar cacheStore (Map.delete key store) 
          return Nothing 
    handleException store e = do
      putMVar cacheStore store
      throwIO e

-- | Gets an entry from the given @Cache@. This operation is thread-safe.
--
-- If there's a cache miss because the entry is not present or is stale,
-- the @cacheFetch@ function passed to @createCache@ is used to attempt
-- to fetch the value. 
cacheGet :: (Ord k, MonadUnliftIO m) => k -> Cache k v -> m (Maybe v)
cacheGet key cache = resultGet <$> cacheGetResult key cache 

-- | Puts a value directly into the cache, or clears it if the value passed is @Nothing@.
cachePut :: (Ord k, MonadIO m) => k -> Maybe v -> Cache k v -> m () 
cachePut key (Just value) Cache{..} = do
  ne <- newEntry value
  store <- takeMVar cacheStore
  putMVar cacheStore $ Map.insert key ne store
cachePut key Nothing Cache{..} = do 
  store <- takeMVar cacheStore
  putMVar cacheStore $ Map.delete key store
