{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveDataTypeable, DuplicateRecordFields, PatternSynonyms, TemplateHaskell, TypeFamilies, NoGeneralisedNewtypeDeriving #-}

module Prosumma.Types (
  AppName,
  CRUDOperation(..),
  DotName,
  IP,
  Name,
  OS(..),
  ipFromSockAddr,
  pattern DotName,
  pattern Name,
  module Prosumma.Types.Localization,
  module Prosumma.Types.TimeZone
) where

import Data.Aeson
import Data.CaseInsensitive (CI, FoldCase)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Net.Types
import Network.Socket
import Prosumma.AWS.DynamoDB
import Prosumma.Textual
import Prosumma.Types.Localization
import Prosumma.Types.TimeZone
import RIO
import Servant

import qualified Data.CaseInsensitive as CI
import qualified Data.String.Conversions.Monomorphic as S
import qualified Net.IP as IP

instance (FoldCase a, FromJSON a) => FromJSON (CI a) where
  parseJSON value = CI.mk <$> parseJSON value

instance ToJSON a => ToJSON (CI a) where
  toJSON = toJSON . CI.original

data CRUDOperation = Create | Read | Update | Delete deriving (Eq, Ord, Enum, Show, Read, Generic, Hashable, Data, Typeable, NFData)

nameRegex :: Text
nameRegex = "^[a-z][a-z0-9]*$"

newtype Name = Name' Text deriving (Eq, Ord, Generic, Hashable, Data, Typeable, NFData)

pattern Name :: Text -> Name
pattern Name name <- Name' name

instance Show Name where
  show = showTextual

instance FromText Name where
  fromText = ifMatchTextual nameRegex Name'

instance ToText Name where
  toText (Name' name) = name

instance IsString Name where
  fromString = fromStringTextual "Name"

instance ToJSON Name where
  toJSON = toJSON . toText

instance FromJSON Name where
  parseJSON = parseJSONTextual "Name"

instance FromField Name where
  fromField = fromFieldTextual "Name"

instance ToField Name where
  toField = toFieldTextual

instance ToHttpApiData Name where
  toUrlPiece = toText

instance FromHttpApiData Name where
  parseUrlPiece = parseUrlPieceTextual "Name"

type instance TypeAttributeConstructor Name = 'ConstructorS

instance FromAttributeConstructorType Name where
  fromAttributeConstructorType = fromText

instance FromScalarAttributeValue Name

instance FromAttributeValue Name where
  fromAttributeValue = fromScalarAttributeValue

instance ToAttributeConstructorType Name where
  toAttributeConstructorType = toText

instance ToScalarAttributeValue Name

instance ToAttributeValue Name where
  toAttributeValue = toScalarAttributeValue

type AppName = Name

dotNameRegex :: Text
dotNameRegex = "^[a-z][a-z0-9]*(\\.[a-z][a-z0-9]*)*$"

newtype DotName = DotName' Text deriving (Eq, Ord, Generic, Hashable, Data, Typeable, NFData)

pattern DotName :: Text -> DotName
pattern DotName name <- DotName' name

instance Show DotName where
  show = showTextual

instance FromText DotName where
  fromText = ifMatchTextual dotNameRegex DotName'

instance ToText DotName where
  toText (DotName' name) = name

instance IsString DotName where
  fromString = fromStringTextual "DotName"

instance ToJSON DotName where
  toJSON = toJSON . toText

instance FromJSON DotName where
  parseJSON = parseJSONTextual "DotName"

instance FromField DotName where
  fromField = fromFieldTextual "DotName"

instance ToField DotName where
  toField = toFieldTextual

instance ToHttpApiData DotName where
  toUrlPiece = toText

instance FromHttpApiData DotName where
  parseUrlPiece = parseUrlPieceTextual "DotName"

type instance TypeAttributeConstructor DotName = 'ConstructorS

instance FromAttributeConstructorType DotName where
  fromAttributeConstructorType = fromText

instance FromScalarAttributeValue DotName

instance FromAttributeValue DotName where
  fromAttributeValue = fromScalarAttributeValue

instance ToAttributeConstructorType DotName where
  toAttributeConstructorType = toText

instance ToScalarAttributeValue DotName

instance ToAttributeValue DotName where
  toAttributeValue = toScalarAttributeValue

ipFromSockAddr :: SockAddr -> Maybe IP
ipFromSockAddr (SockAddrInet _ hostAddress) = let (t1, t2, t3, t4) = hostAddressToTuple hostAddress in Just $ IP.ipv4 t1 t2 t3 t4
ipFromSockAddr (SockAddrInet6 _ _ hostAddress _) = let (t1, t2, t3, t4, t5, t6, t7, t8) = hostAddress6ToTuple hostAddress in Just $ IP.ipv6 t1 t2 t3 t4 t5 t6 t7 t8
ipFromSockAddr _ = Nothing

instance FromText IP where
  fromText = IP.decode

instance ToText IP where
  toText = IP.encode

instance FromField IP where
  fromField = fromFieldTextual "IP"

instance ToField IP where
  toField = toFieldTextual

data OS = OSiOS | OSiPhoneOS | OSiPadOS | OSAndroid | OSmacOS | OSWindows deriving (Eq, Ord, Enum, Read, Show, Generic, Hashable, Data, Typeable, NFData)

instance FromText OS where
  fromText = readMaybe . ("OS" ++) . S.toString

instance ToText OS where
  toText = S.toStrictText . drop 2 . show

instance IsString OS where
  fromString = fromStringTextual "OS"

instance FromField OS where
  fromField = fromFieldTextual "OS"

instance ToField OS where
  toField = toFieldTextual

instance ToJSON OS where
  toJSON = toJSONTextual

instance FromJSON OS where
  parseJSON = parseJSONTextual "OS"
