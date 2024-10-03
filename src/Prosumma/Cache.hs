{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module Prosumma.Cache (
  Cache,
  Entry(..),
  Fetch,
  Outcome(..),
  Result,
  Store,
  accessedL,
  addedL,
  cacheDelete,
  cacheGet,
  cacheGetEither,
  cacheGetEntry,
  cacheGetMaybe,
  cacheGetResult,
  cachePut,
  hitsL,
  newCache,
  setReap,
  valueL,
  withSizeLimitBy,
  withTTLBy,
) where

import Control.Composition
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
  hits     :: !Integer,
  value    :: !v
} deriving (Eq, Ord, Show)

makeLensesL ''Entry

newEntry :: UTCTime -> v -> Entry v
newEntry time = Entry time time 0

type Store k v = HashMap k (Entry v) 
type Reap m k v = Store k v -> m (Store k v)

data Cache k v = Cache {
  _fetch      :: !(Fetch k v),
  _lock       :: !(WLock (Store k v)),
  _reapThread :: !(IORef (Maybe (Async ())))
}

makeLensesL ''Cache

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
setReap :: MonadUnliftIO m => Cache k v -> Int -> Maybe (Reap m k v) -> m ()
setReap cache interval reap = do
  let ref = cache^.reapThreadL
  reapThread <- readIORef ref
  for_ reapThread cancel
  case reap of  
    Nothing -> writeIORef ref Nothing
    Just reap -> do
      newReapThread <- async $ forever $ do
        threadDelay interval
        withWLock_ (cache^.lockL) reap
      writeIORef ref $ Just newReapThread

data Outcome = Cached | Fetched deriving (Eq, Show)
type Result v = (v, Outcome)

-- | Gets a `Result (Entry v)` from the @Cache@, propagating any exceptions for the underlying fetch function.
cacheGetEntry :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m (Result (Entry v))
cacheGetEntry k cache = do
  now <- getCurrentTime
  let updateEntry entry = entry & accessedL .~ now & hitsL %~ (+1)
  let wlock = cache^.lockL
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
          v <- liftIO $ (cache^.fetchL) k
          let entry = newEntry now v & hitsL %~ (+1)
          let newStore = HashMap.insert k entry store
          return (newStore, (entry, Fetched))

-- | Gets an `Result v` from the @Cache@, propagating any exceptions from the underlying fetch function.
cacheGetResult :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m (Result v) 
cacheGetResult k cache = cacheGetEntry k cache <&> over _1 value 

cacheGet :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m v 
cacheGet k cache = cacheGetResult k cache <&> fst

cacheGetEither :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m (Either SomeException v)
cacheGetEither k cache = catchAny (cacheGet k cache <&> Right) (return . Left) 

cacheGetMaybe :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m (Maybe v)
cacheGetMaybe k cache = hush <$> cacheGetEither k cache 

cacheDelete :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m ()
cacheDelete k cache = withWLock_ (cache^.lockL) $ return . HashMap.delete k

cachePut :: (Hashable k, MonadUnliftIO m) => k -> v -> Cache k v -> m ()
cachePut k v cache = do
  now <- getCurrentTime
  let entry = newEntry now v
  withWLock_ (cache^.lockL) $ return . HashMap.insert k entry

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
