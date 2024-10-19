module Prosumma.Logging (
  readLogLevel
) where

import Data.Text.Read
import Prosumma.Textual
import RIO hiding (Reader)
import RIO.Text
import Text.Printf

instance FromText LogLevel where
  fromText "Debug" = Just LevelDebug
  fromText "Info" = Just LevelInfo
  fromText "Warn" = Just LevelWarn
  fromText "Error" = Just LevelError
  fromText text = Just $ LevelOther text
  
instance ToText LogLevel where
  toText LevelDebug = "Debug"
  toText LevelInfo = "Info"
  toText LevelWarn = "Warn"
  toText LevelError = "Error"
  toText (LevelOther text) = text

-- | Doesn't support 'LevelOther'.
readLogLevel :: Reader LogLevel
readLogLevel text
  | Just remainder <- stripPrefix "Debug" text = Right (LevelDebug, remainder)
  | Just remainder <- stripPrefix "Info" text = Right (LevelInfo, remainder)
  | Just remainder <- stripPrefix "Warn" text = Right (LevelWarn, remainder)
  | Just remainder <- stripPrefix "Error" text = Right (LevelError, remainder)
  | otherwise = Left $ printf "'%s' is not a valid LogLevel." text
