module Prosumma.Messaging.App (
  App,
  AppContext,
  newAppContext
) where

import Amazonka
import Database.PostgreSQL.Simple
import Data.Pool
import Formatting
import Prosumma
import Prosumma.AWS
import Prosumma.Messaging.Email
import Prosumma.PG
import Prosumma.Settings
import RIO
import RIO.Text

data AppContext = AppContext {
  acEnv :: !Env,
  acConnectionPool :: !(Pool Connection),
  acEmailTemplates :: !(Set EmailTemplate),
  acSourceEmails :: !SourceEmails,
  acLogFunc :: !LogFunc
}

instance ConnectionRunner AppContext where
  runConnection AppContext{..} = runConnection acConnectionPool

instance HasAWSEnv AppContext where
  getAWSEnv AppContext{..} = acEnv

instance HasEmailTemplates AppContext where
  getEmailTemplates AppContext{..} = acEmailTemplates

instance HasLogFunc AppContext where
  logFuncL = lens acLogFunc (\ctx acLogFunc -> ctx{acLogFunc})

instance HasSourceEmails AppContext where
  getSourceEmails AppContext{..} = acSourceEmails

messagingSettingsTableSuffix :: Text
messagingSettingsTableSuffix = ".messaging.settings"

scanMessagingSettings :: Text -> RIO Env ByteString 
scanMessagingSettings environment = do
  let table = environment `append` messagingSettingsTableSuffix
  scanSettings table $ \read -> read "connection string" 

newAppContext :: (MonadIO m, HasLogFunc env, MonadReader env m) => m AppContext 
newAppContext = do
  acLogFunc <- asks (^.logFuncL)
  logInfo $ displayText "Reading from OS environment in order to create AppContext."
  logInfo $ displayText "Discovering AWS credentials."
  acEnv <- liftIO $ newEnv discover
  vpc <- envString Nothing "PROSUMMA_VPC"
  namespace <- envString Nothing "PROSUMMA_NAMESPACE"
  let environment = intercalate "." [vpc, namespace]
  logInfo $ uformat ("Prosumma Environment is " % stext % ".") environment
  logInfo $ uformat ("Scanning settings from DynamoDB table " % stext % stext % ".") environment messagingSettingsTableSuffix
  (connectionString, acEmailTemplates) <- runRIO acEnv $ (,) <$> scanMessagingSettings environment <*> listEmailTemplates
  let connect = connectPostgreSQL connectionString
  logInfo $ displayText "Creating connection pool."
  acConnectionPool <- liftIO $ createPool connect close 1 600 10
  logInfo $ displayText "Listing source emails."
  acSourceEmails <- runRIO (PG acConnectionPool acLogFunc) listSourceEmails
  logInfo $ displayText "Successfully created AppContext."
  return AppContext{..}

type App = RIO AppContext
