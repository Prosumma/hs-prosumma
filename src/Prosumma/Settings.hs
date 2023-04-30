{-# LANGUAGE DataKinds, RankNTypes, TypeApplications #-}

module Prosumma.Settings (
  settings,
  lookupErrorIncorrectType,
  lookupErrorKeyNotFound,
  lookupSetting,
  readSettings,
  scanSettings,
  ReadSetting(..),
  Setting(..),
  SettingsReadException(..),
  SettingsScanException(..),
  WriteSetting
) where

import Amazonka.DynamoDB
import Control.Monad.Except
import Data.Generics.Product
import Data.Either.Extra
import Prosumma.AWS
import Prosumma.Textual
import Prosumma.Util
import RIO
import Text.Printf

import qualified RIO.HashMap as HM

data Setting = S !Text | N !Integer | B !Bool deriving (Eq, Show)

type WriteSetting = AttributeValue -> Maybe Setting

settingS :: WriteSetting 
settingS value = S <$> value ^. (field @"s")

settingN :: WriteSetting 
settingN value = N <$> (value ^. (field @"n") >>= fromText)

settingB :: WriteSetting
settingB value = B <$> value ^. (field @"bool")

setting :: WriteSetting 
setting value = coalesce Nothing $ map (\f -> f value) [settingS, settingN, settingB]

-- | Converts a @[HashMap Text AttributeValue]@ to @[HashMap Text Setting]@.
--
-- Each @HashMap@ in the array must consist of a pair, one of which is keyed
-- as "name" and the other as "value". Anything else is ignored.
settings :: [HashMap Text AttributeValue] -> HashMap Text Setting
settings [] = mempty
settings (row:rows) = getRow <> getRows 
  where
    getSetting key = HM.lookup key row >>= setting
    getRows = settings rows
    getRow = case getSetting "name" of 
      Just (S key) -> fromMaybe mempty (getSetting "value" <&> HM.singleton key)
      _other -> mempty 

-- | A simple typeclass for converting a @Setting@ to its underlying type.
class ReadSetting a where
  readSetting :: Setting -> Maybe a

instance ReadSetting Text where
  readSetting (S text) = Just text
  readSetting _other = Nothing

instance ReadSetting Integer where
  readSetting (N integer) = Just integer
  readSetting (S text) = fromText text 
  readSetting _other = Nothing

instance ReadSetting Int where
  readSetting (N integer) = readMaybe $ show integer
  readSetting (S text) = fromText text 
  readSetting _other = Nothing

instance ReadSetting Bool where
  readSetting (B bool) = Just bool
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
lookupSetting :: ReadSetting a => HashMap Text Setting -> Text -> Either String a
lookupSetting m key = maybeToEither keyNotFound setting >>= maybeToEither incorrectType . readSetting
  where
    setting = HM.lookup key m
    keyNotFound = printf lookupErrorKeyNotFound key
    incorrectType = printf lookupErrorIncorrectType key

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
readSettings :: [HashMap Text AttributeValue] -> ((forall s. ReadSetting s => Text -> Either String s) -> Either String a) -> Either String a 
readSettings items make = make lookup
  where
    hm = settings items
    lookup :: ReadSetting s => Text -> Either String s 
    lookup = lookupSetting hm

data SettingsScanException = SettingsScanException !Text !Int deriving (Show, Typeable) 
instance Exception SettingsScanException

data SettingsReadException = SettingsReadException !Text !String deriving (Show, Typeable)
instance Exception SettingsReadException

scanSettings :: (HasAWSEnv env, MonadReader env m, MonadThrow m, MonadUnliftIO m) =>
  Text -> ([HashMap Text AttributeValue] -> Either String a) -> m a 
scanSettings table make = do
  response <- sendAWSThrowOnError (SettingsScanException table) $ newScan table 
  case make <$> response ^. (field @"items") of
    Just (Right settings) -> return settings
    Just (Left e) -> throwM $ SettingsReadException table e
    Nothing -> throwM $ SettingsReadException table "The table is empty." 