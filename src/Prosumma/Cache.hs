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
  cacheGets,
  cacheGetResult,
  cacheGetResults,
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

data Result v = Cached !v | Fetched !Bool !(FetchResult v) deriving Show

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

storeGetResult
  :: (Hashable k, MonadUnliftIO m)
  => k -> Maybe TTL -> Fetch k v -> Store k v -> m (Result v, Store k v)
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
  >>= storePut key (maybeToEither noException maybeValue)
  >>= putMVar cacheStore

cacheDelete :: (Hashable k, MonadIO m) => k -> Cache k v -> m ()
cacheDelete key = cachePut key Nothing

cacheGetResult :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m (Result v)
cacheGetResult key Cache{..} = do
  store <- takeMVar cacheStore
  (result, resultStore) <- storeGetResult key cacheTTL cacheFetch store
  putMVar cacheStore resultStore
  return result

cacheGetResults :: (Hashable k, MonadUnliftIO m, Foldable f) => f k -> Cache k v -> m (HashMap k (Result v))
cacheGetResults keys Cache{..} = do
  store <- takeMVar cacheStore
  (result, resultStore) <- storeGetResults keys cacheTTL cacheFetch store
  putMVar cacheStore resultStore
  return result

cacheGet :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m (Maybe v)
cacheGet key cache = resultToMaybe <$> cacheGetResult key cache

cacheGets :: (Hashable k, MonadUnliftIO m) => [k] -> Cache k v -> m (HashMap k (Maybe v)) 
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