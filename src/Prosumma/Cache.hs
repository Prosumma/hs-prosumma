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
  cacheGetIntent,
  cacheGetResult,
  cachePut,
  clearCache,
  createCache,
  newCache,
  resultToMaybe,
  setCache,
  Cache,
  Result(..),
  TTL
) where

import Prosumma.Util
import RIO 
import RIO.Time

import qualified RIO.HashMap as HM

type FetchResult v = Either SomeException (Maybe v)
type Fetch k v = k -> IO (FetchResult v)
type TTL = NominalDiffTime

data Entry v = Entry {
  entryWhen :: !UTCTime,
  entryValue :: !(Maybe v)
}

type Store k v = HashMap k (Entry v)

data Cache k v = Cache {
  cacheStore :: !(MVar (Store k v)),
  cacheTTL :: !(Maybe TTL),
  cacheFetch :: !(Fetch k v)
}

newEntry :: MonadIO m => Maybe v -> m (Entry v)
newEntry v = liftIO $ Entry <$> getCurrentTime <*> pure v

isEntryStale :: Maybe TTL -> UTCTime -> Entry v -> Bool
isEntryStale  Nothing _ _ = False
isEntryStale (Just ttl) now Entry{..} = diffUTCTime now entryWhen > ttl

isEntryStaleM :: MonadIO m => Maybe TTL -> Entry v -> m Bool
isEntryStaleM ttl entry = do 
  now <- liftIO getCurrentTime
  return $ isEntryStale ttl now entry

data Result v = Cached !(Maybe v) | Fetched !Bool !(FetchResult v) 
type Intent = Result

resultToMaybe :: Result v -> Maybe v
resultToMaybe (Cached maybeValue) = maybeValue
resultToMaybe _ = Nothing

storeGetIntent :: (Hashable k, MonadIO m) => k -> Maybe TTL -> Store k v -> m (Intent v)
storeGetIntent key ttl store = case HM.lookup key store of
  Just entry@Entry{..} -> do 
    isStale <- isEntryStaleM ttl entry
    return $ if isStale then Fetched True (Right entryValue) else Cached entryValue
  Nothing -> return $ Fetched False (Right Nothing)

storePut :: (Hashable k, MonadIO m) => k -> Maybe (Maybe v) -> Store k v -> m (Store k v)
storePut key (Just entryValue) store = newEntry entryValue <&> \ne -> HM.insert key ne store
storePut key Nothing store = return $ HM.delete key store

storeGetResult :: (Hashable k, MonadIO m) => k -> Maybe TTL -> Fetch k v -> Store k v -> m (Result v, Store k v)
storeGetResult key ttl fetch store = do
  intent <- storeGetIntent key ttl store
  case intent of
    Cached v -> return (Cached v, store)
    Fetched isStale _ -> do
      result <- liftIO $ fetch key
      newStore <- storePut key (hush result) store
      return (Fetched isStale result, newStore)

cacheGetIntent :: (Hashable k, MonadIO m) => k -> Cache k v -> m (Intent v)
cacheGetIntent key Cache{..} = do 
  store <- takeMVar cacheStore
  intent <- storeGetIntent key cacheTTL store
  putMVar cacheStore store
  return intent

cachePut :: (Hashable k, MonadIO m) => k -> Maybe (Maybe v) -> Cache k v -> m ()
cachePut key maybeValue Cache{..} = takeMVar cacheStore >>= storePut key maybeValue >>= putMVar cacheStore

cacheGetResult :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m (Result v)
cacheGetResult key Cache{..} = do
  store <- takeMVar cacheStore
  (result, newStore) <- catchAny (storeGetResult key cacheTTL cacheFetch store) (handleException store)
  putMVar cacheStore newStore
  return result
  where
    handleException store e = putMVar cacheStore store >> throwIO e

cacheGet :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m (Maybe v)
cacheGet key cache = resultToMaybe <$> cacheGetResult key cache

setCache :: (Hashable k, MonadIO m) => HashMap k (Maybe v) -> Cache k v -> m ()
setCache store Cache{..} = do
  now <- lifIO getCurrentTime
  void $ swapMVar cacheStore $ HM.map (Entry now) store

clearCache :: (Hashable k, MonadIO m) => Cache k v -> m ()
clearCache = setCache mempty

createCache :: (Hashable k, MonadIO m) => Maybe TTL -> Fetch k v -> m (Cache k v)
createCache cacheTTL cacheFetch = do
  cacheStore <- newMVar mempty
  return Cache{..}

newCache :: (Hashable k, MonadIO m) => Maybe TTL -> Fetch k v -> HashMap k (Maybe v) -> m (Cache k v)
newCache ttl fetch store = do 
  cache <- createCache ttl fetch
  setCache store cache
  return cache