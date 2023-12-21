{-# LANGUAGE FlexibleInstances, GADTs #-}

module Prosumma.PG.QueryRunner (
  ConnectionPool,
  SQLQuery(..),
  QueryRunner(..)
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
  execute :: MonadIO m => SQLQuery -> c -> m Int64 
  query :: (MonadIO m, FromRow r) => SQLQuery -> c -> m [r]
  log :: (MonadReader env m, HasLogFunc env, MonadIO m) => SQLQuery -> c -> m ()

instance QueryRunner Connection where
  execute (SQLQuery sql) conn = liftIO $ PG.execute_ conn sql
  execute (ParameterizedSQLQuery sql params) conn = liftIO $ PG.execute conn sql params
  query (SQLQuery sql) conn = liftIO $ PG.query_ conn sql
  query (ParameterizedSQLQuery sql params) conn = liftIO $ PG.query conn sql params
  log sql conn = do
    logFunc <- ask
    runRIO logFunc $ liftIO (formatSQLQuery conn sql) >>= logDebugS "sql" . display

type ConnectionPool = Pool Connection

withPool :: MonadIO m => ConnectionPool -> (Connection -> IO a) -> m a
withPool pool action = liftIO $ withResource pool action 

instance QueryRunner ConnectionPool where
  execute sql pool = withPool pool (liftIO . execute sql) 
  query sql pool = withPool pool (liftIO . query sql)
  log sql pool = do 
    logFunc <- ask
    withPool pool $ runRIO logFunc . log sql
