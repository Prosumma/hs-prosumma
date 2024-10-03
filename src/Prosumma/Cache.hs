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
  withSizeLimitBy,
  withTTLBy,
) where

import Prelude (putStrLn)
import Prosumma.Util
import Prosumma.WLock
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

-- | Gets an entry from the @Cache@, propagating any exceptions from the underlying fetch function.
--
-- If you don't want exceptions from the underlying fetch function, then you can easily wrap this
-- function, e.g.,
-- > hushGetResult k cache = flip catchAny (return . Nothing) $ Just <$> cacheGetResult k cache
cacheGetResult :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m (Result v) 
cacheGetResult k cache = do
  now <- getCurrentTime
  let wlock = cache^.lockL
  store <- readWLock wlock 
  case HashMap.lookup k store of
    Just entry -> do
      void $ async $ withWLock_ wlock $ return . updateAccessed now entry
      return $ (entry^.valueL, Cached)
    Nothing -> withWLock wlock $ \store -> do
      case HashMap.lookup k store of 
        Just entry -> do
          let newStore = updateAccessed now entry store 
          return (newStore, (entry^.valueL, Fetched)) 
        Nothing -> do
          v <- liftIO $ (cache^.fetchL) k
          let entry = newEntry now v
          let newStore = HashMap.insert k entry store
          return (newStore, (v, Fetched))
  where
    updateAccessed now entry = HashMap.update (const $ Just $ entry & accessedL .~ now) k

cacheGet :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m v 
cacheGet k cache = fst <$> cacheGetResult k cache 

cacheDelete :: (Hashable k, MonadUnliftIO m) => k -> Cache k v -> m ()
cacheDelete k cache = withWLock_ (cache^.lockL) $ return . HashMap.delete k

cachePut :: (Hashable k, MonadUnliftIO m) => k -> v -> Cache k v -> m ()
cachePut k v cache = do
  now <- getCurrentTime
  let entry = newEntry now v
  withWLock_ (cache^.lockL) $ return . HashMap.insert k entry

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
