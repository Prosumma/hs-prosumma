module Prosumma.Tagged (
  module Data.Tagged
) where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Data.Tagged
import Prosumma.Textual
import RIO

instance FromText a => FromText (Tagged s a) where
  fromText text = Tagged <$> fromText text 

instance ToText a => ToText (Tagged s a) where
  toText (Tagged a) = toText a

instance ToField a => ToField (Tagged s a) where
  toField (Tagged a) = toField a

instance FromField a => FromField (Tagged s a) where
  fromField field mData = Tagged <$> fromField field mData
