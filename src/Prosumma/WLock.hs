{-# LANGUAGE BangPatterns, OverloadedRecordDot #-}

module Prosumma.WLock (
  WLock,  
  newWLock,
  readWLock,
  withWLock,
  withWLock_,
) where

import RIO

data WLock a = WLock {
  ref :: !(IORef a),
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
