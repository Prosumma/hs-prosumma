{-# LANGUAGE TemplateHaskell #-}

-- TODO: These belong in the Prosumma library for general use.

module Prosumma.Types (
  AppName,
  Language(..),
  Localization(..),
  Name(..),
  Region(..),
  localizationLanguage,
  localizationRegion
) where

import Data.Aeson
import Data.Aeson.Types
import Data.Default
import Control.Lens (makeLenses)
import Data.String.Conversions
import Database.PostgreSQL.Simple.FromField
import Prosumma.Textual
import RIO
import Text.Printf
import Text.Regex.TDFA

import qualified RIO.Text as Text

newtype Language = Language Text deriving (Eq, Ord)

instance Show Language where
  show (Language language) = convertString language

languageRegex :: Text
languageRegex = "^[a-z]{2}$"

instance Textual Language where
  fromText text = if text =~ languageRegex
    then Just $ Language text
    else Nothing
  toText (Language language) = language

instance IsString Language where
  fromString string = case fromText (fromString string) of 
    Just language -> language
    Nothing -> error $ printf "'%s' is not a valid language code." string

instance Default Language where
  def = "en"

instance ToJSON Language where
  toJSON (Language language) = toJSON language

instance FromJSON Language where
  parseJSON (String language) = case fromText language of
    Just language -> return language
    Nothing -> fail $ printf "'%s' is not a valid language." language
  parseJSON invalid = typeMismatch "Language" invalid

instance FromField Language where
  fromField = fromJSONField

newtype Region = Region Text deriving (Eq, Ord)

instance Show Region where
  show (Region region) = convertString region

regionRegex :: Text
regionRegex = "^[A-Z]{2}$"

instance Textual Region where
  fromText text = if text =~ regionRegex
    then Just $ Region text
    else Nothing
  toText (Region region) = region

instance IsString Region where
  fromString string = case fromText (fromString string) of
    Just region -> region
    Nothing -> error $ printf "'%s' is not a valid region code." string

instance ToJSON Region where
  toJSON (Region region) = toJSON region

instance FromJSON Region where
  parseJSON (String region) = case fromText region of 
    Just region -> return region
    Nothing -> fail $ printf "'%s' is not a valid region." region
  parseJSON invalid = typeMismatch "Region" invalid

instance FromField Region where
  fromField = fromJSONField

data Localization = Localization {
  _localizationLanguage :: !Language,
  _localizationRegion :: !(Maybe Region)
} deriving (Eq, Ord)

makeLenses ''Localization

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
      toLocalization' (Just [[_match, language, "", ""]]) = Just $ Localization (Language language) Nothing
      toLocalization' (Just [[_match, language, _optional, region]]) = Just $ Localization (Language language) (Just (Region region)) 
      toLocalization' _nomatch = Nothing 
  toText (Localization (Language language) (Just (Region region))) = Text.concat [language, "-", region] 
  toText (Localization (Language language) Nothing) = language

instance IsString Localization where
  fromString string = case fromText (fromString string) of 
    Just localization -> localization
    Nothing -> error $ printf "'%s' is not a valid localization string"
  
instance ToJSON Localization where
  toJSON = toJSON . toText

instance FromJSON Localization where
  parseJSON (String localization) = case fromText localization of
    Just localization -> return localization
    Nothing -> fail $ printf "'%s' is not a valid Localization." localization
  parseJSON invalid = typeMismatch "Localization" invalid

instance FromField Localization where
  fromField = fromJSONField

nameRegex :: Text
nameRegex = "^[a-z][a-z0-9]*$"

newtype Name = Name Text deriving (Eq, Ord)

instance Show Name where
  show (Name name) = convertString name

instance Textual Name where
  fromText text = if text =~ nameRegex
    then Just $ Name text
    else Nothing
  toText (Name name) = name

instance IsString Name where
  fromString string = case fromText (fromString string) of
    Just name -> name
    Nothing -> error $ printf "'%s' is not a valid Name."

instance ToJSON Name where
  toJSON = toJSON . toText

instance FromJSON Name where
  parseJSON (String name) = case fromText name of
    Just name -> return name
    Nothing -> fail $ printf "'%s' is not a valid Name." name
  parseJSON invalid = typeMismatch "Name" invalid

instance FromField Name where
  fromField = fromJSONField

type AppName = Name
