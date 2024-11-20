{-# LANGUAGE GADTs #-}

module Prosumma.SQLite.QueryRunner (
  QueryRunner(..),
  SQLQuery(..),
  TransactionRunner(..)
) where

import Database.SQLite.Simple (Connection, ToRow, Query, FromRow)
import RIO

import qualified Database.SQLite.Simple as SQLite

data SQLQuery where
  SQLQuery :: Query -> SQLQuery
  ParameterizedSQLQuery :: ToRow q => Query -> q -> SQLQuery

-- | A type which can invoke SQL queries.
class QueryRunner c where
  execute :: (MonadReader env m, MonadIO m) => SQLQuery -> c -> m () 
  query :: (MonadReader env m, MonadIO m, FromRow r) => SQLQuery -> c -> m [r]
  setTrace :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m) => c -> Maybe (Text -> m ()) -> m ()

instance QueryRunner Connection where
  execute (SQLQuery sql) conn = liftIO $ SQLite.execute_ conn sql
  execute (ParameterizedSQLQuery sql params) conn = liftIO $ SQLite.execute conn sql params
  query (SQLQuery sql) conn = liftIO $ SQLite.query_ conn sql
  query (ParameterizedSQLQuery sql params) conn = liftIO $ SQLite.query conn sql params
  setTrace conn Nothing = liftIO $ SQLite.setTrace conn Nothing
  setTrace conn (Just log) = withRunInIO $ \runInIO -> SQLite.setTrace conn (Just $ runInIO . log)

class TransactionRunner c where
  transact :: MonadUnliftIO m => c -> m a -> m a

instance TransactionRunner Connection where
  transact conn action = withRunInIO $ \runInIO -> SQLite.withTransaction conn (runInIO action)
 