{-# LANGUAGE FlexibleInstances, GADTs #-}

module Prosumma.PG.QueryRunner (
  ConnectionPool,
  QueryRunner(..),
  SQLQuery(..),
  withConnectionPool
) where

import Data.Pool
import Database.PostgreSQL.Simple (formatQuery, Connection)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import Prosumma.Textual
import RIO hiding (log)

import qualified Database.PostgreSQL.Simple as PG

formatSQLQuery :: Connection -> SQLQuery -> IO Text
formatSQLQuery _ (SQLQuery sql) = return $ toText $ fromQuery sql
formatSQLQuery conn (ParameterizedSQLQuery sql q) = toText <$> formatQuery conn sql q

data SQLQuery where
  SQLQuery :: Query -> SQLQuery
  ParameterizedSQLQuery :: ToRow q => Query -> q -> SQLQuery

class QueryRunner c where
  execute :: MonadUnliftIO m => SQLQuery -> c -> m Int64 
  query :: (MonadUnliftIO m, FromRow r) => SQLQuery -> c -> m [r]
  log :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m) => SQLQuery -> c -> m ()

instance QueryRunner Connection where
  execute (SQLQuery sql) conn = liftIO $ PG.execute_ conn sql
  execute (ParameterizedSQLQuery sql params) conn = liftIO $ PG.execute conn sql params
  query (SQLQuery sql) conn = liftIO $ PG.query_ conn sql
  query (ParameterizedSQLQuery sql params) conn = liftIO $ PG.query conn sql params
  log sql conn = liftIO (formatSQLQuery conn sql) >>= logDebugS "sql" . display

type ConnectionPool = Pool Connection

withConnectionPool :: MonadUnliftIO m => ConnectionPool -> (Connection -> m a) -> m a
withConnectionPool pool action = withRunInIO $ \runInIO -> withResource pool $ runInIO . action 

instance QueryRunner ConnectionPool where
  execute sql pool = withConnectionPool pool (execute sql)
  query sql pool = withConnectionPool pool (query sql)
  log sql pool = withConnectionPool pool (log sql)
