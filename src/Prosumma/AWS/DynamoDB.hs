{-# LANGUAGE DataKinds, ExistentialQuantification, FlexibleContexts, RankNTypes, TypeApplications #-}

module Prosumma.AWS.DynamoDB (
  getItem,
  getItem',
  putItem,
  query',
  readItem,
  scan,
  scan',
  DynamoDBException(..),
  DynamoDBItemException(..),
  FromAttributeResult(..),
  FromAttributeValue(..),
  FromItem(..),
  ItemError(..),
  TableItem,
  ToAttributeValue(..),
  ToItem(..),
  (=:)
) where

import Amazonka hiding (query)
import Amazonka.DynamoDB
import Control.Applicative
import Data.Generics.Product
import Data.Typeable (cast)
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

instance FromAttributeValue Bool where
  fromAttributeValue (Just (BOOL b)) = Success b
  fromAttributeValue Nothing = MissingValue
  fromAttributeValue _ = InvalidFormat

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

data ItemError = ItemEmptyError | ItemMissingValue Text | ItemInvalidFormat Text (Maybe AttributeValue) deriving (Eq, Show, Typeable)
instance Exception ItemError

instance Semigroup ItemError where
  ItemEmptyError <> e = e
  e <> _ = e

instance Monoid ItemError where
  mempty = ItemEmptyError

instance Alternative (Either ItemError) where
  empty = Left mempty 
  Left _ <|> a = a
  a <|> _ = a

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

instance ToAttributeValue Bool where
  toAttributeValue = BOOL

instance ToAttributeValue Int where
  toAttributeValue = N . Text.pack . show

instance ToAttributeValue Integer where
  toAttributeValue = N . Text.pack . show

instance ToAttributeValue UUID where
  toAttributeValue = S . Text.toLower . UUID.toText

instance ToAttributeValue ByteString where
  toAttributeValue = B . Base64

instance ToAttributeValue a => ToAttributeValue (Maybe a) where
  toAttributeValue (Just a) = toAttributeValue a
  toAttributeValue Nothing = NULL

infixr 8 =:

(=:) :: ToAttributeValue a => Text -> a -> (Text, AttributeValue)
key =: value = (key, toAttributeValue value)

class ToItem a where
  toItem :: a -> TableItem

data DynamoDBException = forall e. Exception e => DynamoDBException e deriving Typeable
instance Exception DynamoDBException

instance Show DynamoDBException where
  show (DynamoDBException e) = show e

newtype DynamoDBItemException = DynamoDBItemException ItemError deriving (Show, Typeable)

instance Exception DynamoDBItemException where
  toException = toException . DynamoDBException
  fromException x = do
    DynamoDBException e <- fromException x
    cast e

-- | A helper function to perform a `GetItem` request.
getItem' :: (HasAWSEnv env, MonadReader env m, MonadUnliftIO m, FromItem i, MonadThrow m) => GetItem -> m (Maybe i)
getItem' gi = do
  res <- sendAWSThrowHTTPStatus gi
  case res^.(field @"item") of
    Just item -> case fromItem item of
      Left e -> throwM $ DynamoDBItemException e
      Right i -> return $ Just i
    Nothing -> return Nothing

-- | A helper function to perform a `GetItem` request with a key and deserialize the result into a Haskell type.
getItem :: (HasAWSEnv env, MonadReader env m, MonadUnliftIO m, FromItem i, MonadThrow m) => Text -> TableItem -> m (Maybe i)
getItem table key = getItem' $ newGetItem table & (field @"key") .~ key

-- | A helper function to perform a `Scan` request and deserialize the results into Haskell types.
scan' :: (HasAWSEnv env, HasLogFunc env, MonadReader env m, MonadUnliftIO m, FromItem i, MonadThrow m) => Scan -> m [i]
scan' s = do
  res <- sendAWSThrowHTTPStatus s
  case res^.(field @"items") of
    Just items -> do
      -- A single bad record could spike our data, so we log it and move on. 
      let (errors, items') = partitionEithers $ map fromItem items
      forM_ errors $ logError . displayShow
      return items'
    Nothing -> return []

-- | A helper function to perform a `Scan` request and deserialize the results into Haskell types.
scan :: (HasAWSEnv env, HasLogFunc env, MonadReader env m, MonadUnliftIO m, FromItem i, MonadThrow m) => Text -> m [i]
scan = scan' . newScan

-- | A helper function to perform a simple `PutItem` request.
--
-- If you need more advanced capabilities such as conditions, don't use this. Just do the request directly.
putItem :: (HasAWSEnv env, MonadReader env m, MonadUnliftIO m, ToItem i, MonadThrow m) => Text -> i -> m ()
putItem table item = void $ sendAWSThrowHTTPStatus req
  where
    req :: PutItem
    req = newPutItem table & (field @"item") .~ toItem item

query' :: (HasAWSEnv env, HasLogFunc env, MonadReader env m, MonadUnliftIO m, FromItem i, MonadThrow m) => Query -> m [i]
query' q = do
  res <- sendAWSThrowHTTPStatus q
  let (errors, items') = partitionEithers $ map fromItem $ res^.(field @"items")
  -- A single bad record could spike our data, so we log it and move on. 
  forM_ errors $ logError . displayShow
  return items'