{-# LANGUAGE RankNTypes #-}

module Prosumma.AWS.DynamoDB2 (
  readItem,
  FromAttributeValue(..),
  FromAttributeResult(..),
  FromItem(..),
  TableItem,
  ToAttributeValue(..),
  ToItem(..),
  (=:)
) where

import Amazonka
import Amazonka.DynamoDB
import Data.UUID (UUID)
import Prosumma.AWS
import Prosumma.Textual
import RIO

import qualified Data.UUID as UUID
import qualified RIO.HashMap as HashMap
import qualified RIO.Text as Text

type TableItem = HashMap Text AttributeValue

data FromAttributeResult a = Success a | MissingValue | InvalidFormat deriving (Eq, Show)

maybeToAttributeResult :: Maybe a -> FromAttributeResult a
maybeToAttributeResult (Just a) = Success a
maybeToAttributeResult Nothing = InvalidFormat

instance Functor FromAttributeResult where
  fmap f (Success a) = Success $ f a
  fmap _ MissingValue = MissingValue
  fmap _ InvalidFormat = InvalidFormat

instance Applicative FromAttributeResult where
  pure = Success
  (Success f) <*> (Success a) = Success $ f a
  MissingValue <*> _ = MissingValue 
  InvalidFormat <*> _ = InvalidFormat
  _ <*> MissingValue = MissingValue
  _ <*> InvalidFormat = InvalidFormat
  
instance Monad FromAttributeResult where
  return = pure
  (Success a) >>= f = f a
  MissingValue >>= _ = MissingValue
  InvalidFormat >>= _ = InvalidFormat

class FromAttributeValue a where
  fromAttributeValue :: Maybe AttributeValue -> FromAttributeResult a

instance FromAttributeValue Text where
  fromAttributeValue (Just (S text)) = Success text
  fromAttributeValue Nothing = MissingValue
  fromAttributeValue _ = InvalidFormat

instance FromAttributeValue String where
  fromAttributeValue a = Text.unpack <$> fromAttributeValue a

instance FromAttributeValue Int where
  fromAttributeValue (Just (N text)) = maybeToAttributeResult $ fromText text
  fromAttributeValue Nothing = MissingValue
  fromAttributeValue _ = InvalidFormat

instance FromAttributeValue Integer where
  fromAttributeValue (Just (N text)) = maybeToAttributeResult $ fromText text
  fromAttributeValue Nothing = MissingValue
  fromAttributeValue _ = InvalidFormat

instance FromAttributeValue UUID where
  fromAttributeValue (Just (S uuid)) = maybeToAttributeResult $ UUID.fromText uuid
  fromAttributeValue Nothing = MissingValue
  fromAttributeValue _ = InvalidFormat

instance FromAttributeValue ByteString where
  fromAttributeValue (Just (B (Base64 b))) = Success b
  fromAttributeValue Nothing = MissingValue
  fromAttributeValue _ = InvalidFormat

instance FromAttributeValue a => FromAttributeValue (Maybe a) where
  fromAttributeValue (Just NULL) = Success Nothing
  fromAttributeValue Nothing = Success Nothing
  fromAttributeValue a = case fromAttributeValue a of
    Success a -> Success (Just a)
    MissingValue -> Success Nothing
    InvalidFormat -> InvalidFormat

data ItemError = ItemMissingValue Text | ItemInvalidFormat Text (Maybe AttributeValue)

class FromItem a where
  fromItem :: TableItem -> Either ItemError a

lookupAttributeValue :: FromAttributeValue a => TableItem -> Text -> Either ItemError a
lookupAttributeValue item key = case fromAttributeValue value of
  Success a -> return a
  InvalidFormat -> Left $ ItemInvalidFormat key value
  MissingValue -> Left $ ItemMissingValue key
  where
    value = HashMap.lookup key item

readItem :: ((forall v. FromAttributeValue v => Text -> Either ItemError v) -> Either ItemError a) -> TableItem -> Either ItemError a
readItem read item = read $ lookupAttributeValue item

class ToAttributeValue a where
  toAttributeValue :: a -> AttributeValue

instance ToAttributeValue Text where
  toAttributeValue = S

instance ToAttributeValue String where
  toAttributeValue = S . Text.pack

instance ToAttributeValue Int where
  toAttributeValue = N . Text.pack . show

instance ToAttributeValue Integer where
  toAttributeValue = N . Text.pack . show

instance ToAttributeValue UUID where
  toAttributeValue = S . Text.toLower . UUID.toText

instance ToAttributeValue ByteString where
  toAttributeValue = B . Base64

infixr 8 =:

(=:) :: ToAttributeValue a => Text -> a -> (Text, AttributeValue)
key =: value = (key, toAttributeValue value)

class ToItem a where 
  toItem :: a -> TableItem
