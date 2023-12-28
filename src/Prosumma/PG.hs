{-# LANGUAGE FlexibleInstances, GADTs, NamedFieldPuns, RecordWildCards #-}

module Prosumma.PG (
  close,
  connect,
  connectPostgreSQL,
  execute_,
  execute,
  parseConnectInfo,
  query_,
  query,
  query1_,
  query1,
  runPG,
  value1_,
  value1,
  withTransaction,
  withTransaction_,
  Connection,
  ConnectInfo(..),
  ConnectionPool,
  Only(..),
  PG(..),
  Query,
  QueryRunner,
  TransactionRunner,
  RPG,
) where

import Control.Composition
import Data.Char
import Data.Functor
import Data.Attoparsec.Text
import Database.PostgreSQL.Simple (close, connect, connectPostgreSQL, defaultConnectInfo, Connection, ConnectInfo(..), FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Types
import Data.List.Safe
import Data.String.Conversions.Monomorphic
import Prosumma.PG.QueryRunner (ConnectionPool, QueryRunner, SQLQuery(..), TransactionRunner)
import RIO hiding (log)

import qualified Prosumma.PG.QueryRunner as QR
import qualified RIO.Map as Map

-- | Contains the minimum fields needed to call one of the query functions
-- exported from this module, such as @execute@, @query@, @value1@, etc.
--
-- Of course, @r@ must implement @QueryRunner@ for it to work.
data PG r = PG { pgQueryRunner :: r, pgLogFunc :: LogFunc }

type RPG r = RIO (PG r)

runPG :: MonadIO m => r -> LogFunc -> RPG r a -> m a
runPG runner logFunc = runRIO (PG runner logFunc)

instance HasLogFunc (PG r) where
  logFuncL = lens pgLogFunc $ \context pgLogFunc -> context{pgLogFunc}

instance QueryRunner r => QueryRunner (PG r) where
  execute sql PG{..} = QR.execute sql pgQueryRunner
  query sql PG{..} = QR.query sql pgQueryRunner

execute_ 
  :: (MonadReader env m, QueryRunner env, HasLogFunc env, MonadUnliftIO m)
  => Query -> m Int64
execute_ sql = ask >>= QR.execute (SQLQuery sql) 

execute
  :: (MonadReader env m, QueryRunner env, HasLogFunc env, MonadUnliftIO m, ToRow q)
  => Query -> q -> m Int64
execute sql q = ask >>= QR.execute (ParameterizedSQLQuery sql q) 

query_
  :: (MonadReader env m, QueryRunner env, HasLogFunc env, MonadUnliftIO m, FromRow r)
  => Query -> m [r]
query_ sql = ask >>= QR.query (SQLQuery sql) 

query
  :: (MonadReader env m, QueryRunner env, HasLogFunc env, MonadUnliftIO m, ToRow q, FromRow r)
  => Query -> q -> m [r]
query sql q = ask >>= QR.query (ParameterizedSQLQuery sql q) 

query1_
  :: (MonadReader env m, QueryRunner env, HasLogFunc env, MonadUnliftIO m, MonadThrow m, FromRow r)
  => Query -> m r
query1_ = query_ >=> head

query1
  :: (MonadReader env m, QueryRunner env, HasLogFunc env, MonadUnliftIO m, ToRow q, MonadThrow m, FromRow r)
  => Query -> q -> m r
query1 = query >=*> head

value1_
  :: (MonadReader env m, QueryRunner env, HasLogFunc env, MonadUnliftIO m, MonadThrow m, FromField v)
  => Query -> m v
value1_ = (fromOnly <$>) . query1_ 

value1
  :: (MonadReader env m, QueryRunner env, HasLogFunc env, MonadUnliftIO m, MonadThrow m, ToRow q, FromField v)
  => Query -> q -> m v
value1 = (fromOnly <$>) .* query1 

withTransaction :: (MonadReader env m, TransactionRunner env, MonadUnliftIO m) => m a -> m a 
withTransaction action = ask >>= flip QR.transact action

withTransaction_ :: (MonadReader env m, TransactionRunner env, MonadUnliftIO m) => m a -> m () 
withTransaction_ = void . withTransaction 

type ConnectionStringMap = Map Text Text

parseConnectionString :: Text -> Either String ConnectionStringMap
parseConnectionString = parseOnly connectionStringParser

connectionStringParser :: Parser ConnectionStringMap
connectionStringParser = Map.fromList <$> pair `sepBy` space
  where
    pair = (,) <$> key <*> (char '=' *> value)
    key = takeTill (== '=')
    value = quotedValue <|> plainValue
    plainValue = takeTill isSpace
    quotedValue = char '\'' *> escapedString '\'' <* char '\''
    escapedString quoteChar = scan False $ \escaping c ->
        case (escaping, c) of
            (True, _) -> Just False
            (False, ch) -> if ch == quoteChar then Nothing else Just (ch == '\\')

parseConnectInfo :: Text -> Either String ConnectInfo
parseConnectInfo connectionString = do
  keyValues <- parseConnectionString connectionString
  let lookup = lookupIn keyValues
  let host = lookup "localhost" "host" 
  let dbname = lookup "postgres" "dbname"
  let user = lookup "postgres" "user"
  let password = lookup "" "password"
  return defaultConnectInfo {
    connectHost = host,
    connectUser = user,
    connectDatabase = dbname,
    connectPassword = password
  }
  where
    lookupIn :: Map Text Text -> Text -> Text -> String 
    lookupIn keyValues def key = toString $ fromMaybe def $ Map.lookup key keyValues
