{-# LANGUAGE ExistentialQuantification, DataKinds, RankNTypes, TypeApplications #-}

module Prosumma.Settings (
  settings,
  scanSettings,
  SettingsException(..),
  SettingsNotFoundException(..),
  SettingsReadException(..),
) where

import Amazonka.DynamoDB
import Data.Generics.Product
import Data.Typeable (cast)
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

data SettingsException = forall e. Exception e => SettingsException e deriving Typeable
instance Exception SettingsException

instance Show SettingsException where
  show (SettingsException e) = show e

data SettingsReadException = SettingsReadException !Text !ItemError deriving (Eq, Show) 

instance Exception SettingsReadException where 
  toException = toException . SettingsException 
  fromException x = do
    SettingsException e <- fromException x
    cast e

newtype SettingsNotFoundException = SettingsNotFoundException Text deriving (Eq, Show)

instance Exception SettingsNotFoundException where
  toException = toException . SettingsException 
  fromException x = do
    SettingsException e <- fromException x
    cast e

scanSettings :: (HasAWSEnv env, MonadReader env m, MonadThrow m, MonadUnliftIO m, FromItem s) => Text -> m s 
scanSettings table = do 
  resp <- sendAWSThrowOnStatus $ newScan table
  case resp^.(field @"items") of
    Nothing -> throwM $ SettingsNotFoundException table 
    Just items -> do
      let item = settings items
      case fromItem item of
        Left e -> throwM $ SettingsReadException table e 
        Right s -> return s
