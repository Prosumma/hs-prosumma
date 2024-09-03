{-# LANGUAGE OverloadedRecordDot, TemplateHaskell #-}

module Prosumma.SQLite (
  close,
  execute_,
  execute,
  open,
  query_,
  query,
  query1_,
  query1,
  queryRunnerL,
  value1_,
  value1,
  withTransaction,
  withTransaction_,
  Connection,
  Only(..),
  Query,
  QueryRunner,
  SQLite(..),
  TransactionRunner
) where

import Control.Composition
import Database.SQLite.Simple (close, open, Connection, FromRow, ToRow)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.Types
import Data.List.Safe
import Prosumma.SQLite.QueryRunner (QueryRunner, SQLQuery(..), TransactionRunner)
import Prosumma.Util
import RIO

import qualified Prosumma.SQLite.QueryRunner as QR

data SQLite r = SQLite {
  queryRunner :: !r,
  logFunc :: !LogFunc
}

makeLensesL ''SQLite

instance HasLogFunc (SQLite r) where
  logFuncL = Prosumma.SQLite.logFuncL

instance QueryRunner r => QueryRunner (SQLite r) where
  execute sql sqlite = QR.execute sql sqlite.queryRunner
  query sql sqlite = QR.query sql sqlite.queryRunner

execute_ 
  :: (MonadReader env m, QueryRunner env, MonadUnliftIO m)
  => Query -> m () 
execute_ sql = ask >>= QR.execute (SQLQuery sql) 

execute
  :: (MonadReader env m, QueryRunner env, MonadUnliftIO m, ToRow q)
  => Query -> q -> m () 
execute sql q = ask >>= QR.execute (ParameterizedSQLQuery sql q) 

query_
  :: (MonadReader env m, QueryRunner env, MonadUnliftIO m, FromRow r)
  => Query -> m [r]
query_ sql = ask >>= QR.query (SQLQuery sql) 

query
  :: (MonadReader env m, QueryRunner env, MonadUnliftIO m, ToRow q, FromRow r)
  => Query -> q -> m [r]
query sql q = ask >>= QR.query (ParameterizedSQLQuery sql q) 

query1_
  :: (MonadReader env m, QueryRunner env, MonadUnliftIO m, MonadThrow m, FromRow r)
  => Query -> m r
query1_ = query_ >=> head

query1
  :: (MonadReader env m, QueryRunner env, MonadUnliftIO m, ToRow q, MonadThrow m, FromRow r)
  => Query -> q -> m r
query1 = query >=*> head

value1_
  :: (MonadReader env m, QueryRunner env, MonadUnliftIO m, MonadThrow m, FromField v)
  => Query -> m v
value1_ = (fromOnly <$>) . query1_ 

value1
  :: (MonadReader env m, QueryRunner env, MonadUnliftIO m, MonadThrow m, ToRow q, FromField v)
  => Query -> q -> m v
value1 = (fromOnly <$>) .* query1 

withTransaction :: (MonadReader env m, TransactionRunner env, MonadUnliftIO m) => m a -> m a 
withTransaction action = ask >>= flip QR.transact action

withTransaction_ :: (MonadReader env m, TransactionRunner env, MonadUnliftIO m) => m a -> m () 
withTransaction_ = void . withTransaction 