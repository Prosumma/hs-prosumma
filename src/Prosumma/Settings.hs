{-# LANGUAGE DataKinds, TypeApplications #-}

module Prosumma.Settings (settings, Setting(..)) where

import Amazonka.DynamoDB
import Data.Generics.Product
import Data.Text.Read
import Prosumma.Util
import RIO

import qualified RIO.HashMap as HM

data Setting = S !Text | N !Integer deriving (Eq, Show)

settingS :: AttributeValue -> Maybe Setting
settingS value = S <$> value ^. (field @"s")

settingN :: AttributeValue -> Maybe Setting
settingN value = case value ^. (field @"n") >>= maybeFromRight . decimal of
  Just (n, _rem) -> Just (N n)
  Nothing -> Nothing

setting :: AttributeValue -> Maybe Setting
setting value = firstJusts $ map (\f -> f value) [settingS, settingN]

settings :: [HashMap Text AttributeValue] -> HashMap Text Setting
settings [] = mempty
settings (row:rows) = case HM.lookup "name" row >>= setting of
  Just (S key) -> case HM.lookup "value" row >>= setting of
    Just v -> HM.union (HM.singleton key v) (settings rows)
    Nothing -> settings rows
  _other -> settings rows