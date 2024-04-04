{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedRecordDot #-}

-- | Provides a thread-safe in-memory cache.
--
-- Operations which deal with the cache as a whole put "cache" at the end, e.g.,
-- @createCache@, @clearCache@. Operations which deal with individual cache entries
-- put "cache" at the beginning, e.g., @cacheGet@, @cachePut@.
--
-- Order of arguments closely follows those used for @Map@.
-- 
-- Reaping is the process of 
module Prosumma.Cache (
  Cache,
  Entry(..),
  Fetch,
  Outcome(..),
  Reap,
  Result,
  Store,
  cacheDelete,
  cacheGet,
  cacheGetResult,
  cacheGetResults,
  cacheGets,
  cachePut,
  cachePuts,
  createCache,
  newCache,
  reapAlways,
  sizeCache,
  withSizeLimitBy,
  withTTLBy
) where

import Data.Hashable
import Prosumma.Util
import RIO
import RIO.List (sortOn)
import RIO.Time

import qualified RIO.HashMap as HashMap

distantFuture :: UTCTime
distantFuture = UTCTime (fromGregorian 9999 12 31) (secondsToDiffTime 86399)

type Fetch k v = k -> IO v

data Entry v = Entry {
  added    :: !UTCTime,
  accessed :: !UTCTime,
  value    :: !v
} deriving (Eq, Ord, Show)

type Store k v = HashMap k (Entry v)

type Reap k v = Store k v -> IO (Store k v)

withSizeLimitBy :: Hashable k => (Entry v -> UTCTime) -> Int -> Reap k v
withSizeLimitBy attr limit store
  | HashMap.size store <= limit = return store
  | otherwise = return $ HashMap.fromList $ take limit $ sortOn (Down . attr . snd) $ HashMap.toList store

withTTLBy :: Hashable k => (Entry v -> UTCTime) -> NominalDiffTime -> Reap k v
withTTLBy attr ttl store = do
  now <- getCurrentTime
  return $ foldl' (handle now) store (HashMap.keys store)
  where
    alter _ Nothing = Nothing
    alter now (Just entry) = if diffUTCTime now (attr entry) <= ttl 
      then Just entry
      else Nothing
    handle now store k = HashMap.alter (alter now) k store 

reapAlways :: Reap k v
reapAlways _ = return HashMap.empty 

data Cache k v = Cache {
  reap  :: !(Reap k v),
  fetch :: !(Fetch k v),
  store :: !(MVar (Store k v))
}

data Outcome = Cached | Fetched deriving (Eq, Ord, Show)
type Result v = Either SomeException (v, Outcome)

data Sentinel = Sentinel deriving (Eq, Ord, Show, Generic, Typeable, Hashable)

withLockedStore :: MonadIO m => Cache k v -> (Store k v -> m (a, Store k v)) -> m a
withLockedStore cache useStore = do
  store <- takeMVar cache.store
  (result, resultStore) <- useStore store
  putMVar cache.store resultStore
  return result

withLockedStore_ :: MonadIO m => Cache k v -> (Store k v -> m (Store k v)) -> m ()
withLockedStore_ cache useStore = takeMVar cache.store >>= useStore >>= putMVar cache.store

withReapedStore
  :: (MonadUnliftIO m)
  => Reap k v -> (Store k v -> m (a, Store k v)) -> Store k v -> m (a, Store k v)
withReapedStore reap run store = withRunInIO $ \runInIO -> liftIO (reap store) >>= runInIO . run

withReapedStore_
  :: (MonadUnliftIO m)
  => Reap k v -> (Store k v -> m (Store k v)) -> Store k v -> m (Store k v)
withReapedStore_ reap run store = withRunInIO $ \runInIO -> liftIO (reap store) >>= runInIO . run 

storeGetResult'
  :: (Hashable k, MonadUnliftIO m)
  => k -> Fetch k v -> Store k v -> m (Result v, Store k v)
storeGetResult' key fetch store = do
  now <- getCurrentTime
  case HashMap.lookup key store of
    Just entry -> do
      let resultEntry = entry{accessed=now}
      let resultStore = HashMap.update (const $ Just resultEntry) key store
      return (Right (resultEntry.value, Cached), resultStore)
    Nothing -> do
      fetchResult <- liftIO $ catch (Right <$> fetch key) (return . Left)
      case fetchResult of
        Left e -> return (Left e, store)
        Right value -> do
          let entry = Entry now distantFuture value
          let resultStore = HashMap.insert key entry store
          return (Right (value, Fetched), resultStore)

storeGetResult
  :: (Hashable k, MonadUnliftIO m)
  => k -> Reap k v -> Fetch k v -> Store k v -> m (Result v, Store k v)
storeGetResult key reap fetch = withReapedStore reap $ storeGetResult' key fetch 

storeGetResults
  :: (Hashable k, MonadUnliftIO m)
  => Set k -> Reap k v -> Fetch k v -> Store k v -> m (HashMap k (Result v), Store k v)
storeGetResults keys reap fetch = withReapedStore reap $ \store ->
  foldM get (mempty, store) keys
  where
    get (results, store) key = do 
      (result, nextStore) <- storeGetResult' key fetch store
      return (HashMap.insert key result results, nextStore)

storePut'
  :: (MonadUnliftIO m, Hashable k)
  => k -> Maybe v -> Store k v -> m (Store k v)
storePut' key Nothing store = return $ HashMap.delete key store
storePut' key (Just value) store = do
  now <- getCurrentTime
  return $ HashMap.alter (alterEntry now) key store
  where
    alterEntry now Nothing = Just $ Entry now distantFuture value
    alterEntry now (Just oldEntry) = Just $ oldEntry{accessed=now,value}

storePut
  :: (MonadUnliftIO m, Hashable k)
  => Reap k v -> k -> Maybe v -> Store k v -> m (Store k v)
storePut reap key newValue = withReapedStore_ reap $ storePut' key newValue 

storePuts
  :: (MonadUnliftIO m, Hashable k)
  => Reap k v -> HashMap k (Maybe v) -> Store k v -> m (Store k v)
storePuts reap newValues = withReapedStore_ reap $ \store ->
  foldM put store (HashMap.toList newValues)
  where
    put store (key, newValue) = storePut' key newValue store 

storeDelete :: (MonadUnliftIO m, Hashable k) => Reap k v -> k -> Store k v -> m (Store k v)
storeDelete reap key = withReapedStore_ reap $ return . HashMap.delete key 

newStore :: MonadIO m => HashMap k v -> m (Store k v)
newStore store = liftIO getCurrentTime >>= \now -> return $ HashMap.map (Entry now distantFuture) store

cacheGetResult :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m (Result v)
cacheGetResult key cache = withLockedStore cache $ storeGetResult key cache.reap cache.fetch

cacheGetResults :: (Hashable k, MonadUnliftIO m) => Set k -> Cache k v -> m (HashMap k (Result v))
cacheGetResults keys cache = withLockedStore cache $ storeGetResults keys cache.reap cache.fetch

cacheGet :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m (Maybe v)
cacheGet key cache = fmap fst . hush <$> cacheGetResult key cache

cacheGets :: (Hashable k, MonadUnliftIO m) => Set k -> Cache k v -> m [v]
cacheGets keys cache = mapMaybe (fmap fst . hush) . HashMap.elems <$> cacheGetResults keys cache

cachePut 
  :: (MonadUnliftIO m, Hashable k)
  => k -> Maybe v -> Cache k v -> m ()
cachePut key value cache = withLockedStore_ cache $ storePut cache.reap key value

cachePuts
  :: (MonadUnliftIO m, Hashable k)
  => HashMap k (Maybe v) -> Cache k v -> m ()
cachePuts newValues cache = withLockedStore_ cache $ storePuts cache.reap newValues

cacheDelete :: (MonadUnliftIO m, Hashable k) => k -> Cache k v -> m ()
cacheDelete key cache = withLockedStore_ cache $ storeDelete cache.reap key

newCache :: MonadIO m => Reap k v -> Fetch k v -> HashMap k v -> m (Cache k v)
newCache reap fetch store = Cache reap fetch <$> (newStore store >>= newMVar)

createCache :: (MonadIO m, Hashable k) => Reap k v -> Fetch k v -> m (Cache k v)
createCache reap fetch = newCache reap fetch mempty

sizeCache :: MonadUnliftIO m => Cache k v -> m Int
sizeCache cache = withLockedStore cache $ \store -> return (HashMap.size store, store)
