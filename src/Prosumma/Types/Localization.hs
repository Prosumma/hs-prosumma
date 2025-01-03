{-# LANGUAGE DeriveAnyClass, DeriveGeneric, NoGeneralisedNewtypeDeriving, PatternSynonyms, TemplateHaskell #-}

module Prosumma.Types.Localization (
  Language,
  Localization(..),
  Region,
  pattern Language,
  pattern Region,
  languageL,
  newLocalization,
  regionL
) where

import Data.Aeson
import Data.Attoparsec.Text (Parser)
import Data.Default
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Formatting
import Prosumma.Textual
import Prosumma.Util
import RIO
import Servant

import qualified Data.Attoparsec.Text as Atto
import qualified RIO.Text as Text

-- | ISO 639-1 language code
--
-- The safest way to get an instance of this type is
-- to use `fromText` from its `FromText` instance.
--
-- `Language` also implements `IsString`, but this
-- should be used with caution.
newtype Language = Language' Text deriving (Eq, Ord, Generic, Hashable)

parseLanguage :: Parser Language
parseLanguage = Language' . Text.pack <$> Atto.count 2 (Atto.satisfy $ Atto.inClass "a-z")

instance Read Language where
  readPrec = readTextual

instance Show Language where
  show = showTextual

instance Default Language where
  def = "en"

instance FromText Language where
  fromText = parseTextual parseLanguage 

instance ToText Language where
  toText (Language' lang) = lang

instance IsString Language where
  fromString = fromStringTextual "Language"

instance ToJSON Language where
  toJSON = toJSON . toText 

instance FromJSON Language where
  parseJSON = parseJSONTextual "Language"

instance ToField Language where
  toField = toField . toText

instance FromField Language where
  fromField = fromFieldTextual "Language"

instance ToHttpApiData Language where
  toUrlPiece = toText

instance FromHttpApiData Language where
  parseUrlPiece = parseUrlPieceTextual "Language"

{-# COMPLETE Language #-}
pattern Language :: Text -> Language
pattern Language lang <- Language' lang

-- | Region code.
-- This is an ISO 3166-1 alpha-2 region code.
--
-- The safest way to get an instance of this type is
-- to use `fromText` from its `FromText` instance.
--
-- `Region` also implements `IsString`, but this
-- should be used with caution.
newtype Region = Region' Text deriving (Eq, Ord, Generic, Hashable)

parseRegion :: Parser Region
parseRegion = Region' . Text.pack <$> Atto.count 2 (Atto.satisfy $ Atto.inClass "A-Z")

instance Read Region where
  readPrec = readTextual

instance Show Region where
  show = showTextual

instance FromText Region where
  fromText = parseTextual parseRegion 

instance ToText Region where
  toText (Region' region) = region

instance IsString Region where
  fromString = fromStringTextual "Region"

instance ToJSON Region where
  toJSON = toJSON . toText

instance FromJSON Region where
  parseJSON = parseJSONTextual "Region"

instance ToField Region where
  toField = toField . toText

instance FromField Region where
  fromField = fromFieldTextual "Region"

instance ToHttpApiData Region where
  toUrlPiece = toText

instance FromHttpApiData Region where
  parseUrlPiece = parseUrlPieceTextual "Region"

{-# COMPLETE Region #-}
pattern Region :: Text -> Region
pattern Region region <- Region' region

-- | IETF BCP 47 language tag
--
-- I call this a "Localization".
data Localization = Localization {
  language :: !Language,
  region :: !(Maybe Region)
} deriving (Eq, Ord, Generic, Hashable)

makeLensesL ''Localization

newLocalization :: Text -> Maybe Text -> Maybe Localization 
newLocalization lang region = Localization <$> fromText lang <*> (fromText <$> region) 

parseLocalization :: Parser Localization
parseLocalization = Localization
  <$> parseLanguage
  <*> optional (Atto.char '-' *> parseRegion)

instance Read Localization where
  readPrec = readTextual

instance Show Localization where
  show = showTextual

instance Default Localization where
  def = Localization def Nothing

instance FromText Localization where
  fromText = parseTextual parseLocalization 

instance ToText Localization where
  toText (Localization (Language lang) (Just (Region region))) = sformat (stext % "-" % stext) lang region 
  toText (Localization (Language lang) Nothing) = lang 

instance IsString Localization where
  fromString = fromStringTextual "Localization"

instance ToJSON Localization where 
  toJSON = toJSON . toText

instance FromJSON Localization where
  parseJSON = parseJSONTextual "Localization"

instance ToField Localization where
  toField = toField . toText

instance FromField Localization where
  fromField = fromFieldTextual "Localization"

instance ToHttpApiData Localization where
  toUrlPiece = toText

instance FromHttpApiData Localization where
  parseUrlPiece = parseUrlPieceTextual "Localization"
