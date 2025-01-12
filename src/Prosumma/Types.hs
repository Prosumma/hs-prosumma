module Prosumma.Types (
  CI
) where

import Data.Aeson
import Data.CaseInsensitive (CI, FoldCase)
import Prosumma.Textual
import RIO
import Servant

import qualified Data.CaseInsensitive as CI

instance (FromText a, FoldCase a) => FromText (CI a) where
  fromText a = CI.mk <$> fromText a

instance ToText a => ToText (CI a) where
  toText = toText . CI.original

instance (FoldCase a, FromJSON a) => FromJSON (CI a) where
  parseJSON value = CI.mk <$> parseJSON value

instance ToJSON a => ToJSON (CI a) where
  toJSON = toJSON . CI.original

instance (FromText a, FoldCase a) => FromHttpApiData (CI a) where
  parseUrlPiece = parseUrlPieceTextual "CI"

instance ToText a => ToHttpApiData (CI a) where
  toUrlPiece = toUrlPiece . toText
