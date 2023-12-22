{-# LANGUAGE FlexibleInstances, GADTs #-}

module Prosumma.PG.QueryRunner (
  ConnectionPool,
  QueryRunner(..),
  SQLQuery(..)
) where

import Database.PostgreSQL.Simple (formatQuery, Connection)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import Prosumma.Pool
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

class QueryRunner c where
  execute :: MonadUnliftIO m => SQLQuery -> c -> m Int64 
  query :: (MonadUnliftIO m, FromRow r) => SQLQuery -> c -> m [r]
  log :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m) => SQLQuery -> c -> m ()

instance QueryRunner Connection where
  execute (SQLQuery sql) conn = liftIO $ PG.execute_ conn sql
  execute (ParameterizedSQLQuery sql params) conn = liftIO $ PG.execute conn sql params
  query (SQLQuery sql) conn = liftIO $ PG.query_ conn sql
  query (ParameterizedSQLQuery sql params) conn = liftIO $ PG.query conn sql params
  log sql conn = liftIO (formatSQLQuery conn sql) >>= logDebugS logSource . display

type ConnectionPool = Pool Connection

instance QueryRunner ConnectionPool where
  execute sql pool = withResource pool (execute sql) 
  query sql pool = withResource pool (query sql)
  log sql pool = withResource pool (log sql)
