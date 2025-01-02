{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveDataTypeable, DuplicateRecordFields #-}

module Prosumma.Push (
  
) where

import Data.Aeson
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Prosumma.Textual
import RIO

import qualified Data.String.Conversions.Monomorphic as S

data PushFormat = APS | FCM deriving (Eq, Ord, Enum, Read, Show, Generic, Hashable, Data, Typeable, NFData)

instance IsString PushFormat where
  fromString = fromStringTextual "PushFormat"

instance FromText PushFormat where
  fromText = readMaybe . S.toString

instance ToText PushFormat where
  toText = S.toStrictText . show

instance FromField PushFormat where
  fromField = fromFieldTextual "PushFormat"

instance ToField PushFormat where
  toField = toFieldTextual

instance ToJSON PushFormat where
  toJSON = toJSONTextual

instance FromJSON PushFormat where
  parseJSON = parseJSONTextual "PushFormat"
