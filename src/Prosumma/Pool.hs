module Prosumma.Pool (
  HasPool(..),
  Pool,
  withPoolResource,
  withResource
) where

import Data.Pool (Pool) 
import RIO

import qualified Data.Pool as Pool

class HasPool a env where 
  getPool :: env -> Pool a

withResource :: MonadUnliftIO m => Pool a -> (a -> m b) -> m b
withResource pool action = withRunInIO $ \runInIO -> Pool.withResource pool (runInIO . action)

withPoolResource :: (MonadReader env m, HasPool a env, MonadUnliftIO m) => (a -> m b) -> m b
withPoolResource action = do
  pool <- asks getPool
  withResource pool action

