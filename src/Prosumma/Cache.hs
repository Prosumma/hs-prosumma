{-# LANGUAGE TemplateHaskell #-}

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
  cacheGetResult,
  cachePut,
  newCache,
  setReap,
  valueL,
  withLockedStore,
  withLockedStore_,
  withSizeLimitBy,
  withTTLBy,
) where

import Prosumma.Util
import RIO
import RIO.List (sortOn)
import RIO.Time

import qualified RIO.HashMap as HashMap

type Fetch k v = k -> IO v

data Entry v = Entry {
  added    :: !UTCTime,
  accessed :: !UTCTime,
  value    :: !v
} deriving (Eq, Ord, Show)

makeLensesL ''Entry

newEntry :: UTCTime -> v -> Entry v
newEntry time = Entry time time

type Store k v = HashMap k (Entry v) 
type Reap m k v = Store k v -> m (Store k v)

data Cache k v = Cache {
  _fetch      :: !(Fetch k v),
  _lock       :: !(MVar (Store k v)),
  _reapThread :: !(IORef (Maybe (Async ())))
}

makeLensesL ''Cache

newCache :: MonadIO m => Fetch k v -> HashMap k v -> m (Cache k v)
newCache fetch init = do
  now <- getCurrentTime
  store <- newMVar $ fmap (newEntry now) init
  reapThread <- newIORef Nothing
  return $ Cache fetch store reapThread 

withLockedStore :: MonadIO m => Cache k v -> (Store k v -> m (a, Store k v)) -> m a
withLockedStore cache use = do
  let lock = cache^.lockL
  store <- takeMVar lock
  (result, resultStore) <- use store
  putMVar lock resultStore
  return result

withLockedStore_ :: MonadIO m => Cache k v -> (Store k v -> m (Store k v)) -> m ()
withLockedStore_ cache use = do 
  let lock = cache^.lockL
  takeMVar lock >>= use >>= putMVar lock

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
        withLockedStore_ cache reap 
      writeIORef ref $ Just newReapThread

data Outcome = Cached | Fetched deriving (Eq, Show)
type Result v = (v, Outcome)

storeGetResult :: (Hashable k, MonadIO m) => k -> Fetch k v -> Store k v -> m (Result v, Store k v)
storeGetResult k fetch store = do
  now <- getCurrentTime
  case HashMap.lookup k store of
    Just entry -> do 
      let newStore = HashMap.update (const $ Just $ entry & accessedL .~ now) k store
      return ((entry^.valueL, Cached), newStore)
    Nothing -> do
      v <- liftIO $ fetch k      
      let newStore = HashMap.insert k (newEntry now v) store
      return ((v, Fetched), newStore)

-- | Gets an entry from the @Cache@, propagating any exceptions from the underlying fetch function.
--
-- If you don't want exceptions from the underlying fetch function, then you can easily wrap this
-- function, e.g.,
-- > hushGetResult k cache = flip catchAny (return . Nothing) $ Just <$> cacheGetResult k cache
cacheGetResult :: (Hashable k, MonadIO m) => k -> Cache k v -> m (Result v) 
cacheGetResult k cache = withLockedStore cache $ storeGetResult k (cache^.fetchL)

cacheGet :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m v 
cacheGet k cache = fst <$> cacheGetResult k cache 

cacheDelete :: (Hashable k, MonadIO m) => k -> Cache k v -> m ()
cacheDelete k cache = withLockedStore_ cache $ return . HashMap.delete k

cachePut :: (Hashable k, MonadIO m) => k -> v -> Cache k v -> m ()
cachePut k v cache = do
  now <- getCurrentTime
  let entry = newEntry now v
  withLockedStore_ cache $ return . HashMap.insert k entry

withSizeLimitBy :: (Monad m, Hashable k) => (Entry v -> UTCTime) -> Int -> Reap m k v
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
