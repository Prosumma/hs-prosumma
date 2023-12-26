{-# LANGUAGE DataKinds, RankNTypes, TypeApplications #-}

module Prosumma.Settings (
  settings,
  readSettings,
  scanSettings,
  SettingsReadException(..),
  SettingsScanException(..),
  TableItem,
) where

import Amazonka.DynamoDB
import Data.Generics.Product
import Prosumma.AWS
import Prosumma.AWS.DynamoDB
import RIO

import qualified RIO.HashMap as HM

-- | Converts a @[HashMap Text AttributeValue]@ to @HashMap Text AttributeValue@.
--
-- Each @HashMap@ in the array must consist of a pair, one of which is keyed
-- as "name" and the other as "value". Anything else is ignored.
settings :: [TableItem] -> HashMap Text AttributeValue
settings [] = mempty
settings (row:rows) = getRow <> getRows
  where
    getSetting key = HM.lookup key row
    getRows = settings rows
    getRow = case getSetting "name" of
      Just (S key) -> maybe mempty (HM.singleton key) (getSetting "value")
      _other -> mempty

-- | Helper function to read settings types from DynamoDB tables.
--
-- > data Settings {
-- >   stgsName :: !Text
-- >   stgsAge  :: !Integer
-- > }
-- >
-- > readPersonSettings :: [HashMap Text AttributeValue] -> Maybe Settings
-- > readPersonSettings = readSettings $
-- >   \lookup -> Settings <$> lookup "name" <*> lookup "age"
readSettings
  :: ((forall s. ReadAttributeValue s => Text -> Either String s) -> Either String a)
  -> [TableItem]
  -> Either String a
readSettings make items = make $ lookupAttributeValue (settings items)

data SettingsScanException = SettingsScanException !Text !Int deriving (Show, Typeable)
instance Exception SettingsScanException

data SettingsReadException = SettingsReadException !Text !String deriving (Show, Typeable)
instance Exception SettingsReadException

-- | Helper function to read settings types directly from a DynamoDB table.
--
-- Unlike @readSettings@, this takes the name of the settings table and performs the call,
-- throwing a @SettingsScanException@ if the table cannot be retrieved. 
scanSettings
  :: (HasAWSEnv env, MonadReader env m, MonadThrow m, MonadUnliftIO m)
  => Text -> ((forall s. ReadAttributeValue s => Text -> Either String s) -> Either String a) -> m a
scanSettings table make = do
  response <- sendAWSThrowOnError (SettingsScanException table) $ newScan table
  case response ^. (field @"items") of
    Just items -> case make $ lookupAttributeValue (settings items) of
      Right s -> return s
      Left e -> throwM $ SettingsReadException table e
    Nothing -> throwM $ SettingsReadException table "The table is empty."