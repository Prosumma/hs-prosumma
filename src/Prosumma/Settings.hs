{-# LANGUAGE DataKinds, RankNTypes, TypeApplications #-}

module Prosumma.Settings (settings, lookupSetting, readSettings, Setting(..), ReadSetting(..), WriteSetting) where

import Amazonka.DynamoDB
import Data.Generics.Product
import Data.Text.Read
import Prosumma.Util
import RIO

import qualified RIO.HashMap as HM

data Setting = S !Text | N !Integer deriving (Eq, Show)

type WriteSetting = AttributeValue -> Maybe Setting

settingS :: WriteSetting 
settingS value = S <$> value ^. (field @"s")

settingN :: WriteSetting 
settingN value = case value ^. (field @"n") >>= maybeFromRight . decimal of
  Just (n, _rem) -> Just (N n)
  Nothing -> Nothing

setting :: WriteSetting 
setting value = firstJusts $ map (\f -> f value) [settingS, settingN]

-- | Converts a @[HashMap Text AttributeValue]@ to @[HashMap Text Setting]@.
--
-- Each @HashMap@ in the array must consist of a pair, one of which is keyed
-- as "name" and the other as "value". Anything else is ignored.
settings :: [HashMap Text AttributeValue] -> HashMap Text Setting
settings [] = mempty
settings (row:rows) = case HM.lookup "name" row >>= setting of
  Just (S key) -> case HM.lookup "value" row >>= setting of
    Just v -> HM.singleton key v <> settings rows
    Nothing -> settings rows
  _other -> settings rows

-- | A simple typeclass for converting a @Setting@ to its underlying type.
class ReadSetting a where
  readSetting :: Setting -> Maybe a

instance ReadSetting Text where
  readSetting (S text) = Just text
  readSetting _other = Nothing

instance ReadSetting Integer where
  readSetting (N integer) = Just integer
  readSetting _other = Nothing

-- | Looks up a value in the hashmap and converts it to the target type.
--
-- This is useful for initializing types from DynamoDB tables.
lookupSetting :: ReadSetting a => HashMap Text Setting -> Text -> Maybe a
lookupSetting m key = HM.lookup key m >>= readSetting

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
readSettings :: [HashMap Text AttributeValue] -> ((forall s. ReadSetting s => Text -> Maybe s) -> Maybe a) -> Maybe a
readSettings items make = make lookup
  where
    hm = settings items
    lookup :: ReadSetting s => Text -> Maybe s
    lookup = lookupSetting hm