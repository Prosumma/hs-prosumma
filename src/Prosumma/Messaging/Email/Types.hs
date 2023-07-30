{-# LANGUAGE PatternSynonyms, RecordWildCards, TemplateHaskell #-}

module Prosumma.Messaging.Email.Types (
  EmailTemplate(..),
  EmailTemplateKey(..),
  EmailTemplateNotFound(..),
  HasEmailTemplates(..),
  HasSourceEmails(..),
  SourceEmails,
  SourceEmailNotFound(..),
  TemplateName,
  app,
  getSourceEmail,
  key,
  name,
  localization,
  normalizeEmailTemplate
) where

import Data.String.Conversions
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField hiding (name)
import Prosumma
import RIO
import Text.Email.Parser
import Text.Email.Validate
import Text.Regex.TDFA

import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Text as Text

instance Textual EmailAddress where
  toText = convertString . toByteString
  fromText = emailAddress . convertString

instance FromField EmailAddress where
  fromField = fromFieldTextual "EmailAddress"

type TemplateName = Name

data EmailTemplateKey = EmailTemplateKey {
  etkApp :: !AppName,
  etkName :: !TemplateName
} deriving (Eq, Ord)

makeProsummaLenses ''EmailTemplateKey

emailTemplateKeyRegex :: Text
emailTemplateKeyRegex = "^([a-z0-9]+)_([a-z0-9]+)$"

instance Textual EmailTemplateKey where
  toText EmailTemplateKey{..} = Text.intercalate "_" [toText etkApp, toText etkName]
  fromText text = case text =~~ emailTemplateKeyRegex of
    Just [[_match, app, name]] -> Just $ EmailTemplateKey (unsafeFromText app) (unsafeFromText name) 
    _nomatch -> Nothing

instance IsString EmailTemplateKey where
  fromString = fromStringTextual "EmailTemplateKey"

instance Show EmailTemplateKey where
  show = showTextual

instance FromRow EmailTemplateKey where
  fromRow = EmailTemplateKey <$> field <*> field 

type SourceEmails = Map EmailTemplateKey EmailAddress

class HasSourceEmails a where
  getSourceEmails :: a -> SourceEmails 

newtype SourceEmailNotFound = SourceEmailNotFound EmailTemplateKey deriving (Show)
instance Exception SourceEmailNotFound

-- | Finds a source email for a particular app and email template as represented by an `EmailTemplateKey`.
--
-- First looks for an exact match, then falls back to the "any" template if none is found. If after that no
-- match is found, throws `SourceEmailNotFound`.
getSourceEmail :: (HasSourceEmails env, MonadReader env m, MonadThrow m) => EmailTemplateKey -> m EmailAddress
getSourceEmail originalKey = do 
  emails <- asks getSourceEmails
  getSourceEmail' emails originalKey 
  where
    getSourceEmail' :: MonadThrow m => SourceEmails -> EmailTemplateKey -> m EmailAddress
    getSourceEmail' emails key@(EmailTemplateKey _app "any") = case Map.lookup key emails of
      Just email -> return email
      Nothing -> throwM $ SourceEmailNotFound originalKey 
    getSourceEmail' emails key = case Map.lookup key emails of
      Just email -> return email
      Nothing -> getSourceEmail' emails $ key & name .~ "any" 

data EmailTemplate = EmailTemplate {
  etKey :: !EmailTemplateKey,
  etLocalization :: !Localization
} deriving (Eq, Ord)

pattern ET :: AppName -> TemplateName -> Localization -> EmailTemplate
pattern ET app name localization = EmailTemplate (EmailTemplateKey app name) localization

makeProsummaLenses ''EmailTemplate

emailTemplateRegex :: Text
emailTemplateRegex = "^([a-z0-9]+)_([a-z0-9]+)_([a-z]{2}(-[A-Z]{2})?)$"

instance Textual EmailTemplate where
  toText EmailTemplate{..} = Text.intercalate "_" [toText etKey, toText etLocalization] 
  fromText text = case text =~~ emailTemplateRegex of
    Just [[_match, app, name, localization, _region]] -> Just $
      ET (unsafeFromText app) (unsafeFromText name) (unsafeFromText localization)
    _nomatch -> Nothing 

instance IsString EmailTemplate where
  fromString = fromStringTextual "EmailTemplate"

instance Show EmailTemplate where
  show = showTextual

class HasEmailTemplates a where
  getEmailTemplates :: a -> Set EmailTemplate

newtype EmailTemplateNotFound = EmailTemplateNotFound EmailTemplate deriving (Show)
instance Exception EmailTemplateNotFound

-- | Normalizes an `EmailTemplate` by finding one that best matches the given one.
--
-- Given a template name such as `code`, every template is expected to have a default
-- of `any_code_en`, where "any" is the app name and "en" is the language. To match
-- a template, we first try an exact match. If that doesn't work, we remove the region,
-- if any, and try again. So if we can't find `foo_code_de-DE` we then try `foo_code_de`.
-- If we can't find `foo_code_de`, we look for `foo_code_en`. If we can't find that,
-- we start again at the top using the "any" app: `any_code_de-DE` and start the process
-- again. If we reach `any_code_en` and that is not present, we throw `EmailTemplateNotFound`.
normalizeEmailTemplate :: (MonadThrow m, MonadReader env m, HasEmailTemplates env) => EmailTemplate -> m EmailTemplate
normalizeEmailTemplate original = do
  emailTemplates <- asks getEmailTemplates
  lookup original emailTemplates $ normalizeEmailTemplate' emailTemplates original
  where
    normalizeEmailTemplate' emailTemplates emailTemplate@(ET "any" _name "en") =
      lookup emailTemplate emailTemplates $ throwM (EmailTemplateNotFound original)
    normalizeEmailTemplate' emailTemplates emailTemplate@(ET _app _name "en") =
      lookup emailTemplate emailTemplates $ normalizeEmailTemplate' emailTemplates $ original & key.app .~ "any"
    normalizeEmailTemplate' emailTemplates emailTemplate@(ET _app _name (Localization _language Nothing)) =
      lookup emailTemplate emailTemplates $ normalizeEmailTemplate' emailTemplates $ emailTemplate & localization .~ "en"
    normalizeEmailTemplate' emailTemplates emailTemplate@(ET _app _name (Localization language (Just _region))) =
      lookup emailTemplate emailTemplates $ normalizeEmailTemplate' emailTemplates $ emailTemplate & localization .~ Localization language Nothing 
    normalizeEmailTemplate' _ts _t = throwM (EmailTemplateNotFound original) 
    lookup emailTemplate emailTemplates actionIfNotFound = if emailTemplate `Set.member` emailTemplates
      then return emailTemplate else actionIfNotFound
