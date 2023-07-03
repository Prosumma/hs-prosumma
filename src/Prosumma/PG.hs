{-# LANGUAGE FlexibleInstances, GADTs, NamedFieldPuns, RecordWildCards #-}

module Prosumma.PG (
  close,
  connectPostgreSQL,
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

import Control.Composition
import Data.Functor
import Database.PostgreSQL.Simple (close, connectPostgreSQL, formatQuery, Connection, FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Types
import Data.List.Safe
import Data.Pool
import Prosumma.Logging
import Prosumma.Textual
import Prosumma.Util
import RIO hiding (withLogFunc)

import qualified Database.PostgreSQL.Simple as PG

type ConnectionPool = Pool Connection

-- | A @ConnectionRunner@ is any type which can supply a connection to the @Connection -> IO a@
-- action passed as the second parameter.
--
-- This is designed to be used in the @ReaderT env IO a@ pattern popularized by @RIO@,
-- in which @env@ implements @ConnectionRunner@. Types exported from this module
-- which implement @ConnectionRunner@ are @Connection@, @ConnectionPool@ (i.e., 
-- @Pool Connection@) and @PG@. Implementation for your own types is straightforward.
-- 
-- > getUser :: UUID -> RIO (PG Connection) [User]
-- > getUser id = do
-- >  env <- ask 
-- >  runConnection env $ query "SELECT * FROM \"user\" WHERE id = ?" (Only id) 
-- 
-- Note that the @query@ function used in the example above is the one from @Database.PostgreSQL.Simple@.
-- 
-- However, there's no need to call @runConnection@ directly. Instead, the functions exported from
-- this module should be used, since they take care of calling @runConnection@ for you:
--
-- > getUser :: UUID -> RIO (PG Connection) [User]
-- > getUser id = query "SELECT * FROM \"user\" WHERE id = ?" (Only id)   
class ConnectionRunner r where
  -- | Runs the @Connection -> IO a@ action.
  --
  -- Conforming implementations use @r@ to provide a @Connection@ to the
  -- @IO@ action. See the instances for @ConnectionPool@ and @Connection@.
  runConnection :: MonadIO m => r -> (Connection -> IO a) -> m a

instance ConnectionRunner ConnectionPool where
  runConnection = liftIO .* withResource

instance ConnectionRunner Connection where
  runConnection = liftIO .* (&)

-- | Contains the minimum fields needed to call one of the query functions
-- exported from this module, such as @execute@, @query@, @value1@, etc.
--
-- Of course, @r@ must implement @ConnectionRunner@ for it to work.
data PG r = PG { pgConnectionRunner :: r, pgLogFunc :: LogFunc }

instance HasLogFunc (PG r) where
  logFuncL = lens pgLogFunc $ \context pgLogFunc -> context{pgLogFunc}

instance ConnectionRunner r => ConnectionRunner (PG r) where
  runConnection PG{..} = runConnection pgConnectionRunner

withConnection :: (MonadReader env m, ConnectionRunner env, MonadIO m) => (Connection -> IO a) -> m a
withConnection = ask >>=> runConnection 

logSource :: LogSource
logSource = "SQL"

-- | A GADT whose primary purpose is to hide @q@ from the signature of @run@.
data SQLQuery where
  SQLQuery :: Query -> SQLQuery
  ParameterizedSQLQuery :: ToRow q => Query -> q -> SQLQuery

formatSQLQuery :: Connection -> SQLQuery -> IO Text
formatSQLQuery _ (SQLQuery sql) = return $ toText $ fromQuery sql
formatSQLQuery conn (ParameterizedSQLQuery sql q) = formatQuery conn sql q <&> toText

-- | Logs a @SQLQuery@ and then runs the @Connection -> IO a@ action with @runConnection@. 
-- 
-- This helper function is used by @runSQLQuery@ and @runParameterizedSQLQuery@. 
--
-- Note that the log level must be at least DEBUG for the query to be logged.
run
  :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m)
  => SQLQuery -> (Connection -> IO a) -> m a
run query action = 
  withLogFunc $ \logFunc -> 
    withConnection $ \conn ->
      runRIO (Logger logFunc) $
        liftIO (formatSQLQuery conn query) >>=
        logDebugS logSource . display >>
        liftIO (action conn)

-- | Runs — but first, logs — an unparameterized SQL query. 
runSQLQuery
  :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m)
  => (Connection -> Query -> IO a) -> Query -> m a
runSQLQuery action sql = run (SQLQuery sql) $ flip action sql

-- | Runs — but first, logs — a parameterized SQL query.
runParameterizedSQLQuery
  :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m, ToRow q)
  => (Connection -> Query -> q -> IO a) -> Query -> q -> m a
runParameterizedSQLQuery action sql q = run (ParameterizedSQLQuery sql q) $ slipr action sql q

execute_
  :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m)
  => Query -> m Int64
execute_ = runSQLQuery PG.execute_

execute
  :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m, ToRow q)
  => Query -> q -> m Int64
execute = runParameterizedSQLQuery PG.execute

query_
  :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m, FromRow r)
  => Query -> m [r]
query_ = runSQLQuery PG.query_

query
  :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m, ToRow q, FromRow r)
  => Query -> q -> m [r]
query = runParameterizedSQLQuery PG.query

query1_
  :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m, MonadThrow m, FromRow r)
  => Query -> m r
query1_ = query_ >=> head

query1
  :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m, ToRow q, MonadThrow m, FromRow r)
  => Query -> q -> m r
query1 = query >=*> head

value1_
  :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m, MonadThrow m, FromField v)
  => Query -> m v
value1_ = (fromOnly <$>) . query1_ 

value1
  :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m, MonadThrow m, ToRow q, FromField v)
  => Query -> q -> m v
value1 = (fromOnly <$>) .* query1 