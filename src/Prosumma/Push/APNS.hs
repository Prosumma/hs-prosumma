{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

module Prosumma.Push.APNS (
  Alert(..),
  Aps(..),
  Notification(..),
  alertL,
  apsL,
  badgeL,
  bodyL,
  bodyLocArgsL,
  bodyLocKeyL,
  categoryL,
  contentAvailableL,
  customDataL,
  newContentAvailableNotification,
  newCustomContentAvailableNotification,
  newCustomNotification,
  newNotification,
  soundL,
  subtitleL,
  threadIdL,
  titleL,
  titleLocArgsL,
  titleLocKeyL,
) where
  
import Control.Lens ((?~))
import Data.Aeson
import Data.Default
import Prosumma.Aeson
import Prosumma.Util
import RIO

data Alert = Alert {
  title :: !(Maybe Text),
  titleLocKey :: !(Maybe Text),
  titleLocArgs :: ![Text],
  subtitle :: !(Maybe Text),
  body :: !(Maybe Text),
  bodyLocKey :: !(Maybe Text),
  bodyLocArgs :: ![Text]
} deriving (Eq, Show)

makeLensesL ''Alert

instance Default Alert where
  def = Alert Nothing Nothing mempty Nothing Nothing Nothing mempty

isBodyOnly :: Alert -> Bool
isBodyOnly Alert{..} 
  | isNothing body = False
  | othersEmpty = True
  | otherwise = False 
  where  
    othersEmpty = and [
        isNothing title, 
        isNothing titleLocKey, 
        null titleLocArgs, 
        isNothing subtitle, 
        isNothing bodyLocKey, 
        null bodyLocArgs
      ]

instance ToJSON Alert where
  toJSON alert@Alert{..}
    | isBodyOnly alert = toJSON body 
    | otherwise = stripJSON (ofAll InBoth) $ object [
          "title" .= title, 
          "title-loc-key" .= titleLocKey, 
          "title-loc-args" .= titleLocArgs, 
          "subtitle" .= subtitle, 
          "body" .= body, 
          "loc-key" .= bodyLocKey, 
          "loc-args" .= bodyLocArgs 
        ]

data Aps = Aps  {
  alert :: !Alert, 
  contentAvailable :: !Bool,
  badge :: !(Maybe Word),
  sound :: !(Maybe Text),
  category :: !(Maybe Text),
  threadId :: !(Maybe Text)
} deriving (Eq, Show)

makeLensesL ''Aps

instance Default Aps where
  def = Aps def False Nothing Nothing Nothing Nothing

instance ToJSON Aps where
  toJSON Aps{..} = stripJSON (ofAll InBoth) $ object $ [ "alert" .= alert  ] <> contentAvailableKeys
    where
      contentAvailableKeys
        | contentAvailable = [ "content-available" .= (1 :: Int) ]
        | otherwise = []

data Notification a = Notification {
  aps :: !Aps,
  customData :: !a
} deriving (Eq, Show)

makeLensesL ''Notification

instance Default a => Default (Notification a) where
  def = Notification def def

instance ToJSONPairs a => ToJSON (Notification a) where
  toJSON Notification{..} = object $ [ "aps" .= aps ] <> toJSONPairs customData

newCustomNotification :: Text -> a -> Notification a
newCustomNotification body customData = Notification def customData & apsL.alertL.bodyL ?~ body 

newNotification :: Text -> Notification ()
newNotification body = newCustomNotification body ()

newCustomContentAvailableNotification :: a -> Notification a
newCustomContentAvailableNotification customData = Notification def customData & apsL.contentAvailableL .~ True

newContentAvailableNotification :: Notification ()
newContentAvailableNotification = newCustomContentAvailableNotification ()
