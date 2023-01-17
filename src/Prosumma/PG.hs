module Prosumma.PG (
  HasConnectionPool(..),
  liftPG
) where

import Data.Pool
import Piggy
import RIO

class HasConnectionPool a where
  getConnectionPool :: a -> Pool Connection

liftPG :: (MonadReader env m, HasConnectionPool env, MonadIO m) => PG a -> m a
liftPG pg = do
  pool <- asks getConnectionPool
  liftIO $ withResource pool $ flip withPG pg