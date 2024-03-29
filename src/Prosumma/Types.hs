{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveDataTypeable, PatternSynonyms, TemplateHaskell, TypeFamilies, NoGeneralisedNewtypeDeriving #-}

module Prosumma.Types (
  AppName,
  CRUDOperation(..),
  DotName,
  IANATimeZone,
  IP,
  Language,
  Localization(..),
  Name,
  OS(..),
  PushSystem(..),
  Region,
  ipFromSockAddr,
  language,
  region,
  pattern DotName,
  pattern IANATimeZone,
  pattern Language,
  pattern Name,
  pattern Region
) where

import Data.Aeson
import Data.Default
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Net.Types
import Network.Socket
import Prosumma.AWS.DynamoDB
import Prosumma.Textual
import Prosumma.Util
import RIO
import Servant
import Text.Printf
import Text.Regex.TDFA

import qualified Data.String.Conversions.Monomorphic as S
import qualified Net.IP as IP
import qualified RIO.Text as Text

data CRUDOperation = Create | Read | Update | Delete deriving (Eq, Ord, Enum, Show, Read, Generic, Hashable, Data, Typeable, NFData)

-- | ISO 639-1 language code
--
-- The safest way to get an instance of this type is
-- to use `fromText` from its `Textual` instance.
--
-- `Language` also implements `IsString`, but this
-- should be used with caution.
newtype Language = Language' Text deriving (Eq, Ord, Generic, Hashable, Data, Typeable, NFData)

pattern Language :: Text -> Language
pattern Language lang <- Language' lang

instance Show Language where
  show = showTextual

languageRegex :: Text
languageRegex = "^[a-z]{2}$"

instance Textual Language where
  fromText = ifMatchTextual languageRegex Language'
  toText (Language' language) = language

instance IsString Language where
  fromString = fromStringTextual "Language"

instance Default Language where
  def = "en"

instance ToJSON Language where
  toJSON = toJSON . toText

instance FromJSON Language where
  parseJSON = parseJSONTextual "Language"

instance FromField Language where
  fromField = fromFieldTextual "Language"

instance ToField Language where
  toField = toFieldTextual

instance ToHttpApiData Language where
  toUrlPiece = toText

instance FromHttpApiData Language where
  parseUrlPiece = parseUrlPieceTextual "Language"

newtype Region = Region' Text deriving (Eq, Ord, Generic, Hashable, Data, Typeable, NFData)

pattern Region :: Text -> Region
pattern Region region <- Region' region

instance Show Region where
  show = showTextual

regionRegex :: Text
regionRegex = "^[A-Z]{2}$"

instance Textual Region where
  fromText = ifMatchTextual regionRegex Region'
  toText (Region' region) = region

instance IsString Region where
  fromString = fromStringTextual "Region"

instance ToJSON Region where
  toJSON = toJSON . toText

instance FromJSON Region where
  parseJSON = parseJSONTextual "Region"

instance FromField Region where
  fromField = fromFieldTextual "Region"

instance ToField Region where
  toField = toFieldTextual

instance ToHttpApiData Region where
  toUrlPiece = toText

instance FromHttpApiData Region where
  parseUrlPiece = parseUrlPieceTextual "Region"

data Localization = Localization {
  localizationLanguage :: !Language,
  localizationRegion :: !(Maybe Region)
} deriving (Eq, Ord, Generic, Hashable, Data, Typeable, NFData)

makeProsummaLenses ''Localization

instance Show Localization where
  show (Localization language (Just region)) = printf "%s-%s" (show language) (show region)
  show (Localization language Nothing) = show language

instance Default Localization where
  def = Localization def Nothing

localizationRegex :: Text
-- TDFA doesn't support non-capture groups
localizationRegex = "^([a-z]{2})(-([A-Z]{2}))?$"

instance Textual Localization where
  fromText text = toLocalization' $ text =~~ localizationRegex
    where
      toLocalization' :: Maybe [[Text]] -> Maybe Localization
      toLocalization' (Just [[_match, language, "", ""]]) = Just $ Localization (Language' language) Nothing
      toLocalization' (Just [[_match, language, _optional, region]]) = Just $ Localization (Language' language) (Just (Region' region))
      toLocalization' _nomatch = Nothing
  toText (Localization (Language' language) (Just (Region' region))) = Text.concat [language, "-", region]
  toText (Localization (Language' language) Nothing) = language

instance IsString Localization where
  fromString = fromStringTextual "Localization"

instance ToJSON Localization where
  toJSON = toJSON . toText

instance FromJSON Localization where
  parseJSON = parseJSONTextual "Localization"

instance FromField Localization where
  fromField = fromFieldTextual "Localization"

instance ToField Localization where
  toField = toFieldTextual

instance ToHttpApiData Localization where
  toUrlPiece = toText

instance FromHttpApiData Localization where
  parseUrlPiece = parseUrlPieceTextual "Localization"

type instance TypeAttributeConstructor Localization = 'ConstructorS

instance FromAttributeConstructorType Localization where
  fromAttributeConstructorType = fromText

instance FromScalarAttributeValue Localization

instance FromAttributeValue Localization where
  fromAttributeValue = fromScalarAttributeValue

instance ToAttributeConstructorType Localization where
  toAttributeConstructorType = toText

instance ToScalarAttributeValue Localization

instance ToAttributeValue Localization where
  toAttributeValue = toScalarAttributeValue

nameRegex :: Text
nameRegex = "^[a-z][a-z0-9]*$"

newtype Name = Name' Text deriving (Eq, Ord, Generic, Hashable, Data, Typeable, NFData)

pattern Name :: Text -> Name
pattern Name name <- Name' name

instance Show Name where
  show = showTextual

instance Textual Name where
  fromText = ifMatchTextual nameRegex Name'
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

instance Textual DotName where
  fromText = ifMatchTextual dotNameRegex DotName'
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

instance Textual IP where
  fromText = IP.decode
  toText = IP.encode

instance FromField IP where
  fromField = fromFieldTextual "IP"

instance ToField IP where
  toField = toFieldTextual

newtype IANATimeZone = IANATimeZone' Text deriving (Eq, Ord, Show, Generic, Hashable, Data, Typeable, NFData)

pattern IANATimeZone :: Text -> IANATimeZone
pattern IANATimeZone tz <- IANATimeZone' tz

ianaTimeZoneRegex :: Text
ianaTimeZoneRegex = "^[A-Za-z0-9_-]+/[A-Za-z0-9_-]+$"

instance Textual IANATimeZone where
  toText (IANATimeZone' text) = text
  fromText text = if text =~ ianaTimeZoneRegex
    then return $ IANATimeZone' text
    else Nothing

instance IsString IANATimeZone where
  fromString = fromStringTextual "IANATimeZone"

instance FromJSON IANATimeZone where
  parseJSON = parseJSONTextual "IANATimeZone"

instance ToJSON IANATimeZone where
  toJSON = toJSONTextual

instance FromField IANATimeZone where
  fromField = fromFieldTextual "IANATimeZone"

instance ToField IANATimeZone where
  toField = toFieldTextual

type instance TypeAttributeConstructor IANATimeZone = 'ConstructorS

instance FromAttributeConstructorType IANATimeZone where
  fromAttributeConstructorType = fromText

instance FromScalarAttributeValue IANATimeZone

instance FromAttributeValue IANATimeZone where
  fromAttributeValue = fromScalarAttributeValue

instance ToAttributeConstructorType IANATimeZone where
  toAttributeConstructorType = toText

instance ToScalarAttributeValue IANATimeZone

instance ToAttributeValue IANATimeZone where
  toAttributeValue = toScalarAttributeValue

data OS = OSiOS | OSiPadOS | OSAndroid | OSmacOS | OSWindows deriving (Eq, Ord, Enum, Read, Show, Generic, Hashable, Data, Typeable, NFData)

instance Textual OS where
  fromText = readMaybe . ("OS" ++) . S.toString
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

data PushSystem = SNS | APS | FCM | WNS deriving (Eq, Ord, Enum, Read, Show, Generic, Hashable, Data, Typeable, NFData)

instance IsString PushSystem where
  fromString = fromStringTextual "PushSystem"

instance Textual PushSystem where
  fromText = readMaybe . S.toString
  toText = S.toStrictText . show

instance FromField PushSystem where
  fromField = fromFieldTextual "PushSystem"

instance ToField PushSystem where
  toField = toFieldTextual

instance ToJSON PushSystem where
  toJSON = toJSONTextual

instance FromJSON PushSystem where
  parseJSON = parseJSONTextual "PushSystem"
