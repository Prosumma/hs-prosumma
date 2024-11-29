{-# LANGUAGE FlexibleInstances, GADTs #-}

module Prosumma.PG.QueryRunner (
  ConnectionPool,
  QueryRunner(..),
  SQLQuery(..),
  TransactionRunner(..)
) where

import Prosumma.Pool
import Database.PostgreSQL.Simple (formatQuery, Connection)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import Prosumma.Textual
import RIO hiding (log)

import qualified Database.PostgreSQL.Simple as PG

logSource :: LogSource
logSource = "SQL"

formatSQLQuery :: Connection -> SQLQuery -> IO Text
formatSQLQuery _ (SQLQuery sql) = return $ toText $ fromQuery sql
formatSQLQuery conn (ParameterizedSQLQuery sql q) = toText <$> formatQuery conn sql q

data SQLQuery where
  SQLQuery :: Query -> SQLQuery
  ParameterizedSQLQuery :: ToRow q => Query -> q -> SQLQuery

-- | A type which can invoke SQL queries.
class QueryRunner c where
  execute :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m) => SQLQuery -> c -> m Int64 
  query :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m, FromRow r) => SQLQuery -> c -> m [r]

log :: (MonadReader env m, HasLogFunc env, MonadIO m) => Connection -> SQLQuery -> m ()
log conn sql = liftIO (formatSQLQuery conn sql) >>= logDebugS logSource . display

instance QueryRunner Connection where
  execute q@(SQLQuery sql) conn =
    log conn q >> liftIO (PG.execute_ conn sql)
  execute q@(ParameterizedSQLQuery sql params) conn =
    log conn q >> liftIO (PG.execute conn sql params)
  query q@(SQLQuery sql) conn =
    log conn q >> liftIO (PG.query_ conn sql)
  query q@(ParameterizedSQLQuery sql params) conn =
    log conn q >> liftIO (PG.query conn sql params)

-- | A type which can execute an action within a transaction.
--
-- @ConnectionPool@ should *never* implement this. The types will
-- work, but the semantics won't. Due to how the pool works,
-- a transaction will be started on a random connection, but this
-- does not guarantee that it will be the same connection on which
-- other operations execute.
--
-- Use @withResource@ to get a transaction, then use its implementation
-- of @TransactionRunner@ to work with transactions, e.g.,
--
-- > withResource pool $ \conn -> do
-- >  runRIO (PG conn logFunc) $
-- >    withTransaction $ void $ execute "DELETE FROM foo" 
--
-- This example uses @withTransaction@ which is implemented in terms of
-- @TransactionRunner@.
class TransactionRunner c where
  transact :: MonadUnliftIO m => c -> m a -> m a

instance TransactionRunner Connection where
  transact conn action = withRunInIO $ \runInIO -> PG.withTransaction conn (runInIO action)

type ConnectionPool = Pool Connection

instance QueryRunner ConnectionPool where
  execute sql pool = withResource pool (execute sql) 
  query sql pool = withResource pool (query sql)
