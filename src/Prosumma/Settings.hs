{-# LANGUAGE DataKinds, RankNTypes, TypeApplications #-}

module Prosumma.Settings (
  settings,
  lookupErrorIncorrectType,
  lookupErrorKeyNotFound,
  lookupSetting,
  readSettings,
  scanSettings,
  ReadSetting(..),
  ReadSettings,
  SettingsReadException(..),
  SettingsScanException(..),
  TableItem,
) where

import Amazonka.DynamoDB
import Control.Monad.Except
import Data.Generics.Product
import Data.Either.Extra
import Prosumma.AWS
import Prosumma.Textual
import RIO
import Text.Printf

import qualified RIO.HashMap as HM

type TableItem = HashMap Text AttributeValue

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
      Just (S key) -> fromMaybe mempty (getSetting "value" <&> HM.singleton key)
      _other -> mempty 

-- | A simple typeclass for converting an @AttributeValue@ to its underlying type.
class ReadSetting a where
  readSetting :: AttributeValue -> Maybe a

instance ReadSetting Text where
  readSetting (S text) = Just text
  readSetting _other = Nothing

instance ReadSetting ByteString where
  readSetting = readSetting >=> fromText 

instance ReadSetting Integer where
  readSetting (N integer) = fromText integer 
  readSetting (S text) = fromText text 
  readSetting _other = Nothing

instance ReadSetting Int where
  readSetting (N integer) = fromText integer 
  readSetting (S text) = fromText text 
  readSetting _other = Nothing

instance ReadSetting Bool where
  readSetting (BOOL bool) = Just bool
  readSetting _other = Nothing

instance ReadSetting s => ReadSetting (Maybe s) where
  readSetting = Just . readSetting

lookupErrorKeyNotFound :: String
lookupErrorKeyNotFound = "The key '%s' was not found."

lookupErrorIncorrectType :: String
lookupErrorIncorrectType = "The key '%s' was found but was not the correct type."

-- | Looks up a value in the hashmap and converts it to the target type.
--
-- This is useful for initializing types from DynamoDB tables.
lookupSetting :: ReadSetting a => HashMap Text AttributeValue -> Text -> Either String a
lookupSetting m key = maybeToEither keyNotFound setting >>= maybeToEither incorrectType . readSetting
  where
    setting = HM.lookup key m
    keyNotFound = printf lookupErrorKeyNotFound key
    incorrectType = printf lookupErrorIncorrectType key

type ReadSettings s = ReadSetting s => Text -> Either String s

-- | Helper function to read settings types from DynamoDB tables.
--
-- > data Settings {
-- >   stgsName :: !Text
-- >   stgsAge  :: !Integer
-- > }
-- >
-- > readPersonSettings :: [HashMap Text AttributeValue] -> Maybe Settings
-- > readPersonSettings items = readSettings items $
-- >   \lookup -> Settings <$> lookup "name" <*> lookup "age"
readSettings :: ((forall s. ReadSettings s) -> Either String a) -> [TableItem] -> Either String a 
readSettings make items = make $ lookupSetting (settings items) 

data SettingsScanException = SettingsScanException !Text !Int deriving (Show, Typeable) 
instance Exception SettingsScanException

data SettingsReadException = SettingsReadException !Text !String deriving (Show, Typeable)
instance Exception SettingsReadException

-- | Helper function to read settings types directly from a DynamoDB table.
--
-- Unlike @readSettings@, this takes the name of the settings table and performs the call,
-- throwing a @SettingsScanException@ if the table cannot be retrieved. 
scanSettings :: (HasAWSEnv env, MonadReader env m, MonadThrow m, MonadUnliftIO m) => Text -> ((forall s. ReadSettings s) -> Either String a) -> m a 
scanSettings table make = do
  response <- sendAWSThrowOnError (SettingsScanException table) $ newScan table
  case response ^. (field @"items") of
    Just items -> case make $ lookupSetting (settings items) of
      Right s -> return s
      Left e -> throwM $ SettingsReadException table e
    Nothing -> throwM $ SettingsReadException table "The table is empty."