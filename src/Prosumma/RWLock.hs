{-# LANGUAGE TemplateHaskell #-}

module Prosumma.RWLock (
  RWLock,
  modifyRWLock,
  modifyRWLock_,
  newRWLock,
  readRWLock,
) where

import Data.Tuple.Extra
import Prosumma.Util
import RIO

data RWLock a = RWLock {
  _valueRef :: !(IORef a),
  _writeLock :: !(MVar ())
}

makeLensesL ''RWLock

newRWLock :: MonadIO m => a -> m (RWLock a)
newRWLock value = RWLock <$> newIORef value <*> newMVar ()

readRWLock :: MonadIO m => RWLock a -> m a
readRWLock lock = readIORef $ lock^.valueRefL 

modifyRWLock :: MonadUnliftIO m => RWLock a -> (a -> m (a, b)) -> m b
modifyRWLock lock action = modifyMVarMasked (lock^.writeLockL) $ const $ do
  let ref = lock^.valueRefL  
  value <- readIORef ref
  (newValue, result) <- action value
  writeIORef ref newValue
  return ((), result)

modifyRWLock_ :: MonadUnliftIO m => RWLock a -> (a -> m a) -> m a
modifyRWLock_ lock action = modifyRWLock lock (fmap dupe . action) 
