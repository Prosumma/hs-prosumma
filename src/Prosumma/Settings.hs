{-# LANGUAGE DataKinds, TypeApplications #-}

module Prosumma.Settings (settings, lookupSetting, Setting(..), ReadSetting(..), WriteSetting) where

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

settings :: [HashMap Text AttributeValue] -> HashMap Text Setting
settings [] = mempty
settings (row:rows) = case HM.lookup "name" row >>= setting of
  Just (S key) -> case HM.lookup "value" row >>= setting of
    Just v -> HM.singleton key v <> settings rows
    Nothing -> settings rows
  _other -> settings rows

class ReadSetting a where
  readSetting :: Setting -> Maybe a

instance ReadSetting Text where
  readSetting (S text) = Just text
  readSetting _other = Nothing

instance ReadSetting Integer where
  readSetting (N integer) = Just integer
  readSetting _other = Nothing

lookupSetting :: ReadSetting a => HashMap Text Setting -> Text -> Maybe a
lookupSetting m key = HM.lookup key m >>= readSetting
