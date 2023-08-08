{-# LANGUAGE TemplateHaskell #-}

module Prosumma.Messaging.Push.Types (
  extId,
  snsArn,
  system,
  token,
  userExtId,
  PushSystem(..),
  PushToken(..)
) where

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField hiding (name)
import Database.PostgreSQL.Simple.ToField
import Data.UUID
import Prosumma.Textual
import Prosumma.Util
import RIO

data PushSystem = APNS | Firebase deriving (Eq, Ord, Enum, Show)

instance Textual PushSystem where
  fromText "apns" = Just APNS
  fromText "firebase" = Just Firebase
  fromText _ = Nothing
  toText APNS = "apns"
  toText Firebase = "firebase"

instance FromField PushSystem where
  fromField = fromFieldTextual "PushSystem"

instance ToField PushSystem where
  toField = toFieldTextual

data PushToken = PushToken {
  ptExtId :: UUID,
  ptUserExtId :: UUID,
  ptToken :: Text,
  ptSystem :: PushSystem,
  ptSNSArn :: Maybe Text
} deriving Show 

makeProsummaLenses ''PushToken

-- | Aliases the default (but ugly) name @sNSArn@, which
-- is not exported.
snsArn :: Lens' PushToken (Maybe Text)
snsArn = sNSArn 

instance FromRow PushToken where
  fromRow = PushToken <$> field <*> field <*> field <*> field <*> field