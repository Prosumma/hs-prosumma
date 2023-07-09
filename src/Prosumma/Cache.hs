{-# LANGUAGE RecordWildCards #-}

module Prosumma.Cache (
  createCache,
  cacheGet,
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

cacheGetEntry :: (Ord k, MonadIO m) => k -> Cache k v -> m (Maybe (Entry v))
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
      maybeValue <- liftIO $ cacheFetch key
      case maybeValue of
        Nothing -> do
          putMVar cacheStore store
          return Nothing
        Just value -> do 
          ne <- newEntry value
          putMVar cacheStore $ Map.insert key ne store 
          return $ Just ne

-- | Gets an entry from the given @Cache@. This operation is thread-safe.
--
-- If there's a cache miss because the entry is not present or is stale,
-- the @cacheFetch@ function passed to @createCache@ is used to attempt
-- to fetch the value. 
cacheGet :: (Ord k, MonadIO m) => k -> Cache k v -> m (Maybe v)
cacheGet key cache = (entryValue <$>) <$> cacheGetEntry key cache 