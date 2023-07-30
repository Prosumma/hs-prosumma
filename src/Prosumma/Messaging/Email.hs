module Prosumma.Messaging.Email (
  module Prosumma.Messaging.Email.Types,
  listEmailTemplates,
  listSourceEmails,
  sendCode
) where

import Amazonka.SESV2
import Control.Lens (each, (?~))
import Control.Lens.Prism
import Data.Aeson.Text
import Data.Generics.Product
import Data.String.Conversions 
import Prosumma
import Prosumma.AWS
import Prosumma.Messaging.Email.Types
import Prosumma.PG
import RIO
import Text.Email.Parser

import qualified RIO.Map as Map
import qualified RIO.Set as Set

pageSize :: Int
pageSize = 10

newtype ListEmailTemplatesFailed = ListEmailTemplatesFailed Int deriving (Show)
instance Exception ListEmailTemplatesFailed

listEmailTemplates'
  :: (MonadReader env m, HasAWSEnv env, MonadUnliftIO m, MonadThrow m)
  => Maybe Text -> m [Text]
listEmailTemplates' nextToken = do
  let request = newListEmailTemplates & (field @"nextToken") .~ nextToken & (field @"pageSize") ?~ pageSize 
  response <- sendAWSThrowOnError ListEmailTemplatesFailed request 
  let templates = response^.(field @"templatesMetadata")._Just^..each.(field @"templateName")._Just
  let nextToken = response^.(field @"nextToken")
  if isNothing nextToken 
    then return templates
    else (templates <>) <$> listEmailTemplates' nextToken

listEmailTemplates :: (MonadReader env m, HasAWSEnv env, MonadUnliftIO m, MonadThrow m) => m (Set EmailTemplate) 
listEmailTemplates = listEmailTemplates' Nothing <&> Set.fromList . mapMaybe fromText 

listSourceEmails
  :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m)
  => m (Map EmailTemplateKey EmailAddress)
listSourceEmails = query_ "SELECT app, template, email FROM source_email" <&> toMap
  where
    toMap = Map.fromList . map convertFromRow
    convertFromRow (app, template, email) = (EmailTemplateKey app template, email)

newtype EmailSendFailed = EmailSendFailed Int deriving (Typeable, Show)
instance Exception EmailSendFailed

sendEmail
  :: (MonadReader env m, HasAWSEnv env, HasSourceEmails env, HasEmailTemplates env, MonadUnliftIO m, MonadThrow m)
  => EmailTemplate -> EmailAddress -> Map Text Text -> m ()
sendEmail template to keyValues = do
  normalizedTemplate <- normalizeEmailTemplate template
  sourceEmail <- getSourceEmail $ template^.key 
  let json = convertString $ encodeToLazyText keyValues
  let emailTemplate = newTemplate
        & (field @"templateName") ?~ toText normalizedTemplate 
        & (field @"templateData") ?~ json
  let emailContent = newEmailContent & (field @"template") ?~ emailTemplate
  let destination = newDestination & (field @"toAddresses") ?~ [toText to]
  let request = newSendEmail emailContent
        & (field @"destination") ?~ destination
        & (field @"fromEmailAddress") ?~ toText sourceEmail
  void $ sendAWSThrowOnError EmailSendFailed request

sendCode
  :: (MonadReader env m, HasAWSEnv env, HasSourceEmails env, HasEmailTemplates env, MonadUnliftIO m, MonadThrow m)
  => AppName -> Localization -> EmailAddress -> Text -> Text -> m ()
sendCode app localization to appName code = do 
  let key = EmailTemplateKey app "code"
  let template = EmailTemplate key localization
  sendEmail template to $ "app" <=> appName <> "code" <=> code 
