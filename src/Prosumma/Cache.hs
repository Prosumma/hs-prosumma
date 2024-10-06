{-# LANGUAGE FlexibleContexts, OverloadedRecordDot, TemplateHaskell #-}

module Prosumma.Cache (
  Cache,
  Entry(..),
  Fetch,
  Outcome(..),
  Reap,
  Result,
  Store,
  accessedL,
  accessesL,
  addedL,
  cacheDelete,
  cacheGet,
  cacheGetEither,
  cacheGetEither_,
  cacheGetEntry,
  cacheGetMaybe,
  cacheGetResult,
  cachePeekEntry,
  cachePut,
  cacheSize,
  newCache,
  newReapedCache,
  setReap,
  valueL,
  withSizeLimitBy,
  withTTLBy,
) where

import Prosumma.Util
import Prosumma.WLock
import RIO
import RIO.Lens
import RIO.List (sortOn)
import RIO.Time

import qualified RIO.HashMap as HashMap

type Fetch k v = k -> IO v

data Entry v = Entry {
  added    :: !UTCTime,
  accessed :: !UTCTime,
  accesses :: !Word,
  value    :: !v
} deriving (Eq, Ord, Show)

makeLensesL ''Entry

newEntry :: UTCTime -> v -> Entry v
newEntry time = Entry time time 0

type Store k v = HashMap k (Entry v) 
type Reap m k v = Store k v -> m (Store k v)

data Cache k v = Cache {
  fetch      :: !(Fetch k v),
  lock       :: !(WLock (Store k v)),
  reapThread :: !(IORef (Maybe (Async ())))
}

newCache :: MonadIO m => Fetch k v -> HashMap k v -> m (Cache k v)
newCache fetch init = do
  now <- getCurrentTime
  store <- newWLock $ fmap (newEntry now) init
  reapThread <- newIORef Nothing
  return $ Cache fetch store reapThread 

-- | Reaping is the act of purging stale cache entries.
-- 
-- Technically, this just sets a function that runs periodically on a background thread.
-- It can do anything with the underlying @Store@. It can even add entries. It does not have
-- to reap. But since reaping is the most common use case, it is called @setReap@.
--
-- The interval is expressed in microseconds, so 1_000_000 is 1 second.
setReap :: MonadUnliftIO m => Cache k v -> Word -> Maybe (Reap m k v) -> m ()
setReap cache interval reap = do
  let ref = cache.reapThread
  reapThread <- readIORef ref
  for_ reapThread cancel
  case reap of  
    Nothing -> writeIORef ref Nothing
    Just reap -> do
      newReapThread <- async $ forever $ do
        threadDelay $ fromIntegral interval
        withWLock_ cache.lock reap
      writeIORef ref $ Just newReapThread

data Outcome = Cached | Fetched deriving (Eq, Show)
type Result v = (v, Outcome)

newReapedCache :: MonadUnliftIO m => Fetch k v -> HashMap k v -> Word -> Reap m k v -> m (Cache k v)
newReapedCache fetch init interval reap = do
  cache <- newCache fetch init
  setReap cache interval (Just reap) 
  return cache

-- | "Peeks" a cache entry but does not attempt to fetch. If there is no corresponding entry,
-- `Nothing` is returned. Non-blocking.
cachePeekEntry :: (Hashable k, MonadIO m) => k -> Cache k v -> m (Maybe (Entry v))
cachePeekEntry k cache = HashMap.lookup k <$> readWLock cache.lock 

-- | Gets a `Result (Entry v)` from the @Cache@, propagating any exceptions for the underlying fetch function.
--
-- This function is non-blocking for cached entries and blocking in all other cases.
cacheGetEntry :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m (Result (Entry v))
cacheGetEntry k cache = do
  now <- getCurrentTime
  let updateEntry entry = entry & accessedL .~ max now entry.accessed & accessesL %~ (+1)
  let wlock = cache.lock
  store <- readWLock wlock
  case HashMap.lookup k store of
    Just entry -> do
      let updatedEntry = updateEntry entry 
      void $ async $ withWLock_ wlock $ return . HashMap.update (Just . updateEntry) k
      return (updatedEntry, Cached)
    Nothing -> withWLock wlock $ \store -> do
      case HashMap.lookup k store of
        Just entry -> do
          let updatedEntry = updateEntry entry 
          let newStore = HashMap.update (const $ Just updatedEntry) k store 
          return (newStore, (updatedEntry, Cached))
        Nothing -> do
          v <- liftIO $ cache.fetch k
          let entry = newEntry now v & accessesL %~ (+1)
          let newStore = HashMap.insert k entry store
          return (newStore, (entry, Fetched))

-- | Gets a `Result v` from the @Cache@, propagating any exceptions from the underlying fetch function.
--
-- This function is non-blocking for cached entries and blocking in all other cases.
cacheGetResult :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m (Result v) 
cacheGetResult k cache = cacheGetEntry k cache <&> over _1 value 

-- | Get a value from the @Cache@, propagating any exceptions from the underlying fetch function.
--
-- This function is non-blocking for cached entries and blocking in all other cases.
cacheGet :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m v 
cacheGet k cache = cacheGetResult k cache <&> fst

-- | Gets a value from the @Cache@. If an exception of type `e` is raised, it is stored in `Left`.
-- All other exceptions propagate.
--
-- This function is non-blocking for cached entries and blocking in all other cases.
cacheGetEither :: (Hashable k, MonadUnliftIO m, Exception e) => k -> Cache k v -> m (Either e v)
cacheGetEither k cache = catch (cacheGet k cache <&> Right) (return . Left)

-- | Gets a value from the @Cache@. If an exception is raised, it is stored in `Left` as @SomeException@.
--
-- This function is non-blocking for cached entries and blocking in all other cases.
cacheGetEither_ :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m (Either SomeException v)
cacheGetEither_ = cacheGetEither

-- | Gets a value from the @Cache@, propagating any exceptions from the underlying fetch function.
-- If an exception is raised, `Nothing` is returned.
--
-- This function is non-blocking for cached entries and blocking in all other cases.
cacheGetMaybe :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m (Maybe v)
cacheGetMaybe k cache = hush <$> cacheGetEither_ k cache 

cacheDelete :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m ()
cacheDelete k cache = withWLock_ cache.lock $ return . HashMap.delete k

cachePut :: (Hashable k, MonadUnliftIO m) => k -> v -> Cache k v -> m ()
cachePut k v cache = do
  now <- getCurrentTime
  let entry = newEntry now v
  withWLock_ cache.lock $ return . HashMap.insert k entry
  
cacheSize :: MonadIO m => Cache k v -> m Int
cacheSize cache = HashMap.size <$> readWLock cache.lock 

withSizeLimitBy :: (Monad m, Hashable k, Ord a) => (Entry v -> a) -> Int -> Reap m k v
withSizeLimitBy attr limit store
  | HashMap.size store <= limit = return store
  | otherwise = return $ HashMap.fromList $ take limit $ sortOn (Down . attr . snd) $ HashMap.toList store

withTTLBy :: (MonadIO m, Hashable k) => (Entry v -> UTCTime) -> NominalDiffTime -> Reap m k v
withTTLBy attr ttl store = do
  now <- getCurrentTime
  return $ foldl' (handle now) store (HashMap.keys store)
  where
    alter _ Nothing = Nothing
    alter now (Just entry) = if diffUTCTime now (attr entry) <= ttl 
      then Just entry
      else Nothing
    handle now store k = HashMap.alter (alter now) k store 
