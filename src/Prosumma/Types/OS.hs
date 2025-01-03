{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Prosumma.Types.OS (
  OS(..)
) where

import Data.Aeson
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Prosumma.Textual
import RIO
import Servant

data OS = OSiPadOS | OSiPhoneOS | OSmacOS | OSAndroid deriving (Eq, Ord, Enum, Generic, Hashable)

instance FromText OS where
  fromText "iPadOS" = pure OSiPadOS
  fromText "iPhoneOS" = pure OSiPhoneOS
  fromText "macOS" = pure OSmacOS
  fromText "Android" = pure OSAndroid
  fromText _ = Nothing

instance ToText OS where
  toText OSiPadOS = "iPadOS"
  toText OSiPhoneOS = "iPhoneOS"
  toText OSmacOS = "macOS"
  toText OSAndroid = "Android"

instance Read OS where
  readPrec = readTextual

instance Show OS where
  show = showTextual

instance FromJSON OS where
  parseJSON = parseJSONTextual "OS"

instance ToJSON OS where
  toJSON = toJSONTextual

instance FromField OS where
  fromField = fromFieldTextual "OS"

instance ToField OS where
  toField = toFieldTextual

instance FromHttpApiData OS where
  parseUrlPiece = parseUrlPieceTextual "OS"

instance ToHttpApiData OS where
  toUrlPiece = toText 
