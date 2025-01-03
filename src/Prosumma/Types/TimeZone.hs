{-# LANGUAGE DeriveAnyClass, DeriveGeneric, NoGeneralisedNewtypeDeriving, PatternSynonyms #-}

module Prosumma.Types.TimeZone (
  IANATimeZone,
  pattern IANATimeZone
) where

import Data.Aeson
import Data.Attoparsec.Text (Parser)
import Data.Default
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Prosumma.Textual
import RIO
import Servant

import qualified Data.Attoparsec.Text as Atto
import qualified RIO.Text as Text

-- | An IANA time zone identifier
-- 
-- Note that IANA time zone identifiers are case-sensitive.
newtype IANATimeZone = IANATimeZone' Text deriving (Eq, Ord, Generic, Hashable)

pattern IANATimeZone :: Text -> IANATimeZone
pattern IANATimeZone tz <- IANATimeZone' tz 

instance Default IANATimeZone where
  def = "Etc/UTC" 

instance Read IANATimeZone where
  readPrec = readTextual

instance Show IANATimeZone where
  show = showTextual

parseIANATimeZone :: Parser IANATimeZone
parseIANATimeZone = do
  region <- segment 
  void $ Atto.char '/'
  location <- segment
  Atto.endOfInput
  pure $ IANATimeZone' $ region <> "/" <> location
  where
    segment = Text.pack <$> Atto.many1 (Atto.satisfy $ Atto.inClass "A-Za-z0-9_-")

instance FromText IANATimeZone where
  fromText = parseTextual parseIANATimeZone 

instance ToText IANATimeZone where
  toText (IANATimeZone' tz) = tz

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

instance ToHttpApiData IANATimeZone where
  toUrlPiece = toText

instance FromHttpApiData IANATimeZone where
  parseUrlPiece = parseUrlPieceTextual "IANATimeZone"