{-# LANGUAGE PatternSynonyms, TemplateHaskell #-}

module Prosumma.Types (
  AppName,
  IP,
  Language,
  Localization(..),
  Name,
  Region,
  ipFromSockAddr,
  language,
  region, 
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
import Prosumma.Textual
import Prosumma.Util
import RIO
import Servant
import Text.Printf
import Text.Regex.TDFA

import qualified Net.IP as IP
import qualified RIO.Text as Text

-- | ISO 639-1 language code
--
-- The safest way to get an instance of this type is
-- to use `fromText` from its `Textual` instance.
--
-- `Language` also implements `IsString`, but this
-- should be used with caution.
newtype Language = Language' Text deriving (Eq, Ord, Hashable)

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

newtype Region = Region' Text deriving (Eq, Ord, Hashable)

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
} deriving (Eq, Ord)

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

nameRegex :: Text
nameRegex = "^[a-z][a-z0-9]*$"

newtype Name = Name' Text deriving (Eq, Ord, Hashable)

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

type AppName = Name

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