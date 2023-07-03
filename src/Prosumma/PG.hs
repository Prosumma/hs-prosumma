{-# LANGUAGE FlexibleInstances, GADTs, NamedFieldPuns, RecordWildCards #-}

module Prosumma.PG (
  execute_,
  execute,
  query_,
  query,
  query1_,
  query1,
  value1_,
  value1,
  ConnectionPool,
  ConnectionRunner(..),
  PG(..)
) where

import Database.PostgreSQL.Simple (formatQuery, Connection, FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Types
import Data.List.Safe
import Data.Pool
import Prosumma.Logging
import Prosumma.Textual
import Prosumma.Util
import RIO

import qualified Database.PostgreSQL.Simple as PG

type ConnectionPool = Pool Connection

class ConnectionRunner r where
  runConnection :: MonadIO m => r -> (Connection -> IO a) -> m a

instance ConnectionRunner ConnectionPool where
  runConnection pool action = liftIO $ withResource pool action

instance ConnectionRunner Connection where
  runConnection conn action = liftIO $ action conn

data PG r = PG { pgConnectionRunner :: r, pgLogFunc :: LogFunc }

instance HasLogFunc (PG r) where
  logFuncL = lens pgLogFunc $ \context pgLogFunc -> context{pgLogFunc}

instance ConnectionRunner r => ConnectionRunner (PG r) where
  runConnection PG{..} = runConnection pgConnectionRunner

logSource :: LogSource
logSource = "SQL"

data SQLQuery where
  SQLQuery :: Query -> SQLQuery
  ParameterizedSQLQuery :: ToRow q => Query -> q -> SQLQuery

formatSQLQuery :: Connection -> SQLQuery -> IO Text 
formatSQLQuery _ (SQLQuery sql) = return $ toText $ fromQuery sql
formatSQLQuery conn (ParameterizedSQLQuery sql q) = formatQuery conn sql q <&> toText

run :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m) => SQLQuery -> (Connection -> IO a) -> m a
run query action = do
  env <- ask
  let logFunc = env^.logFuncL
  runConnection env $ \conn -> do
    runRIO (Logger logFunc) $ do
      formattedQuery <- liftIO $ formatSQLQuery conn query
      logDebugS logSource $ display formattedQuery
    liftIO $ action conn 

runSQLQuery :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m) => (Connection -> Query -> IO a) -> Query -> m a
runSQLQuery action sql = run (SQLQuery sql) $ flip action sql

runParameterizedSQLQuery :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m, ToRow q) => (Connection -> Query -> q -> IO a) -> Query -> q -> m a
runParameterizedSQLQuery action sql q = run (ParameterizedSQLQuery sql q) $ slipr action sql q

execute_ :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m) => Query -> m Int64
execute_ = runSQLQuery PG.execute_

execute :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m, ToRow q) => Query -> q -> m Int64
execute = runParameterizedSQLQuery PG.execute

query_ :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m, FromRow r) => Query -> m [r]
query_ = runSQLQuery PG.query_ 

query :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m, ToRow q, FromRow r) => Query -> q -> m [r]
query = runParameterizedSQLQuery PG.query 

query1_ :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m, MonadThrow m, FromRow r) => Query -> m r 
query1_ = query_ >=> head 

query1 :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m, ToRow q, MonadThrow m, FromRow r) => Query -> q -> m r 
query1 = query >=*> head 

value1_ :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m, MonadThrow m, FromField v) => Query -> m v 
value1_ = query1_ >=> head

value1 :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m, MonadThrow m, ToRow q, FromField v) => Query -> q -> m v
value1 = query1 >=*> head