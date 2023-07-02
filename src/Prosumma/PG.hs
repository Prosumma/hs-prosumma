{-# LANGUAGE FlexibleInstances #-}

module Prosumma.PG (
  execute_,
  execute,
  query_,
  query,
  query1_,
  query1,
  value1_,
  value1,
  HasConnectionPool(..),
) where

import Database.PostgreSQL.Simple (formatQuery, Connection, FromRow, Query, Only(..), ToRow)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Types (fromQuery)
import Data.List.Safe
import Data.Pool
import Prosumma.Logging
import Prosumma.Textual
import RIO

import qualified Database.PostgreSQL.Simple as PG

class HasConnectionPool a where
  getConnectionPool :: a -> Pool Connection

instance HasConnectionPool (Pool Connection) where
  getConnectionPool = id

runWithConnection :: (MonadReader env m, HasConnectionPool env, MonadIO m) => (Connection -> IO a) -> m a
runWithConnection action = do
  pool <- asks getConnectionPool
  liftIO $ withResource pool action

sqlLogSource :: LogSource
sqlLogSource = "SQL"

logSQL :: (MonadReader env m, HasLogFunc env, MonadIO m, HasCallStack) => Query -> m ()
logSQL = logDebugS sqlLogSource .  display . toText . fromQuery

logQuery :: (MonadIO m, ToRow q, HasCallStack) => Connection -> LogFunc -> Query -> q -> m ()
logQuery conn logFunc sql q = runRIO (Logger logFunc) $ do
  query <- liftIO $ formatQuery conn sql q
  logDebugS sqlLogSource $ display $ toText query

execute_ :: (MonadReader env m, HasConnectionPool env, HasLogFunc env, MonadIO m) => Query -> m Int64
execute_ sql = do
  logSQL sql
  runWithConnection $ \conn -> PG.execute_ conn sql

query_ :: (MonadReader env m, HasConnectionPool env, HasLogFunc env, MonadIO m, FromRow r) => Query -> m [r]
query_ sql = do
  logSQL sql
  runWithConnection $ \conn -> PG.query_ conn sql

execute :: (MonadReader env m, HasConnectionPool env, HasLogFunc env, MonadIO m, ToRow q) => Query -> q -> m Int64
execute sql q = do
  logFunc <- asks (^.logFuncL)
  runWithConnection $ \conn -> do
    logQuery conn logFunc sql q
    PG.execute conn sql q

query :: (MonadReader env m, HasConnectionPool env, HasLogFunc env, MonadIO m, ToRow q, FromRow r) => Query -> q -> m [r]
query sql q = do
  logFunc <- asks (^.logFuncL)
  runWithConnection $ \conn -> do
    logQuery conn logFunc sql q
    PG.query conn sql q

query1_ :: (MonadReader env m, HasConnectionPool env, HasLogFunc env, MonadIO m, MonadThrow m, FromRow r) => Query -> m r 
query1_ sql = query_ sql >>= head

query1 :: (MonadReader env m, HasConnectionPool env, HasLogFunc env, MonadIO m, MonadThrow m, ToRow q, FromRow r) => Query -> q -> m r 
query1 sql q = query sql q >>= head

value1_ :: (MonadReader env m, HasConnectionPool env, HasLogFunc env, MonadIO m, MonadThrow m, FromField v) => Query -> m v
value1_ sql = fromOnly <$> query1_ sql

value1 :: (MonadReader env m, HasConnectionPool env, HasLogFunc env, MonadIO m, MonadThrow m, ToRow q, FromField v) => Query -> q -> m v 
value1 sql q = fromOnly <$> query1 sql q