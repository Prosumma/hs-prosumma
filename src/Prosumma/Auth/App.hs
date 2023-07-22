{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Prosumma.Auth.App (
  App,
  AppContext,
  HasAWSEnv(..),
  HasMasterKeyArn(..),
  fromEnvironment
) where

import Amazonka
import Data.Pool
import Formatting
import Prosumma
import Prosumma.AWS
import Prosumma.Crypto
import Prosumma.Environment
import Prosumma.Logging
import Prosumma.PG
import Prosumma.Settings
import RIO hiding (withLogFunc)
import RIO.Text

data AppContext = AppContext {
  appVPC :: !Text,
  appNamespace :: !Text,
  appContextMasterKeyArn :: !Text,
  appContextEnv :: !Env,
  appContextConnectionPool :: !ConnectionPool,
  appLogFunc :: !LogFunc
}

instance HasEnvironment AppContext where
  vpc = appVPC
  namespace = appNamespace

instance HasLogFunc AppContext where
  logFuncL = lens appLogFunc (\context appLogFunc -> context{appLogFunc})

type App = RIO AppContext

authSettingsTableSuffix :: Text
authSettingsTableSuffix = ".auth.settings"

scanAuthSettings :: Text -> RIO Env (ByteString, Text)
scanAuthSettings environment = do
  let table = environment `append` authSettingsTableSuffix 
  scanSettings table $
    \read -> (,)
      <$> read "connection string"
      <*> read "master key arn"

fromEnvironment :: (MonadIO m, HasLogFunc env, MonadReader env m) => m AppContext
fromEnvironment = do
  logInfo $ displayText "Reading from OS environment in order to create AppContext."
  logInfo $ displayText "Discovering AWS credentials."
  env <- liftIO $ newEnv discover
  vpc <- envString Nothing "PROSUMMA_VPC"
  namespace <- envString Nothing "PROSUMMA_NAMESPACE"
  let environment = intercalate "." [vpc, namespace]
  logInfo $ uformat ("Prosumma Environment is " % stext % ".") environment
  logInfo $ uformat ("Scanning settings from DynamoDB table " % stext % stext % ".") environment authSettingsTableSuffix
  (connectionString, masterKeyArn) <- runRIO env $ scanAuthSettings environment
  logDebug $ uformat ("Master Key Arn is " % stext) masterKeyArn
  logInfo $ displayText "Creating connection pool."
  pool <- liftIO $ createPool (connectPostgreSQL connectionString) close 1 600 10
  withLogFunc $ \logFunc ->
    void $ runRIO (PG pool logFunc)
      (value1_ "SELECT 1" :: RIO (PG ConnectionPool) Int) 
  logInfo "Successfully connected to database."
  logInfo "Finished creating AppContext."
  return $ AppContext vpc namespace masterKeyArn env pool mempty

instance ConnectionRunner AppContext where
  runConnection AppContext{..} = runConnection appContextConnectionPool 

instance HasAWSEnv AppContext where
  getAWSEnv AppContext{..} = appContextEnv

instance HasMasterKeyArn AppContext where
  getMasterKeyArn AppContext{..} = appContextMasterKeyArn
