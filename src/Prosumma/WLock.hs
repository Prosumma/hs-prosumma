{-# LANGUAGE BangPatterns, OverloadedRecordDot #-}

module Prosumma.WLock (
  WLock,  
  newWLock,
  readWLock,
  withWLock,
  withWLock_,
) where

import RIO

-- | A "write lock" which locks only writes but not reads.
--
-- This lock appears to work fine, but I'm not 100% certain.
-- My understanding is that IO is thread-safe in Haskell in
-- the sense that it can't be interleaved. Assuming this is
-- true, my presumption is that this lock will work, and it
-- certainly appears to.
--
-- At some point I'll likely nuke this in favor of an RWLock
-- provided by some package somewhere.
data WLock a = WLock {
  ref  :: !(IORef a),
  lock :: !(MVar ())
}

newWLock :: MonadIO m => a -> m (WLock a)
newWLock !value = WLock <$> newIORef value <*> newMVar ()

readWLock :: MonadIO m => WLock a -> m a
readWLock wlock = readIORef wlock.ref

withWLock :: MonadUnliftIO m => WLock a -> (a -> m (a, b)) -> m b
withWLock wlock modify = withMVarMasked wlock.lock $ const $ do 
  originalValue <- readIORef wlock.ref
  (newValue, result) <- modify originalValue
  writeIORef wlock.ref $! newValue
  return result 

withWLock_ :: MonadUnliftIO m => WLock a -> (a -> m a) -> m ()
withWLock_ wlock modify = withMVarMasked wlock.lock $ const $ do
  originalValue <- readIORef wlock.ref
  newValue <- modify originalValue
  writeIORef wlock.ref $! newValue
