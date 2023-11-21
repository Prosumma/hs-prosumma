{-# LANGUAGE TemplateHaskell #-}

module Prosumma.Push (
  Message(..),
  Push(..),
  badge,
  category,
  customData,
  message,
  newAlert,
  newPush
) where

import Data.Aeson
import Data.Aeson.Types
import Data.Default
import Prosumma.Aeson
import Prosumma.Util
import RIO

data Message = ContentAvailable | Message !Text | TitledMessage !Text !Text deriving (Eq, Show)

parseMessage :: Object -> Parser Message
parseMessage o = do
  contentAvailable <- fromMaybe False <$> o .:? "content_available"
  if contentAvailable
    then return ContentAvailable
    else do
      title <- o .:? "title"
      body <- o .:? "body"
      case (title, body) of
        (Just title, Just body) -> return $ TitledMessage title body
        (Just title, Nothing) -> return $ Message title
        (Nothing, Just body) -> return $ Message body
        _ -> fail "Either content_available or title or body must be present."

messageToPairs :: Message -> [Pair]
messageToPairs ContentAvailable = ["content_available" .= True]
messageToPairs (Message message) = ["body" .= message]
messageToPairs (TitledMessage title body) = ["title" .= title, "body" .= body]

type MaybeText = Maybe Text
type MaybeInt = Maybe Int

-- | A cross-platform push notification type
data Push = Push {
  pushMessage :: !Message,
  -- | APNS only, ignored by FCM
  pushBadge :: !MaybeInt,
  -- | APNS only, ignored by FCM
  pushCategory :: !MaybeText,
  pushCustomData :: !Object
} deriving (Eq, Show)

makeProsummaLenses ''Push

instance Default Push where
  def = Push ContentAvailable Nothing Nothing mempty

newPush :: Message -> Push
newPush message = def { pushMessage = message } 

newAlert :: Text -> Push
newAlert = newPush . Message 

instance FromJSON Push where
  parseJSON = withObject "Push" $ \o -> 
    Push
      <$> parseMessage o
      <*> o .:? "badge"
      <*> o .:? "category"
      <*> (fromMaybe mempty <$> o .:? "data")

instance ToJSON Push where
  toJSON Push{..} = stripJSON (ofAll InBoth) $ object $ messageToPairs pushMessage <> [
      "badge" .= pushBadge,
      "category" .= pushCategory,
      "data" .= pushCustomData
    ]
