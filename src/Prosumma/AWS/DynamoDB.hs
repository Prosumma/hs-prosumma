{-# LANGUAGE DataKinds, ExistentialQuantification, FlexibleContexts, RankNTypes, TypeApplications #-}

module Prosumma.AWS.DynamoDB (
  AttributeItem,
  FromAttributeValue(..),
  FromTableItem(..),
  TableItem,
  ToTableItem(..),
  ValueError(..),
  getItem,
  getItem',
  putItem,
  putItem',
  readAttributeItem,
  readTableItem,
  scan,
  scanWithFilter,
  writeAttributeItem,
  writeTableItem,
  (=:)
) where

import Amazonka hiding (query)
import Amazonka.DynamoDB
import Control.Applicative
import Control.Monad.Error.Class
import Data.Generics.Product
import Data.Typeable (cast)
import Data.UUID (UUID)
import Prosumma.AWS
import Prosumma.Textual
import RIO
import RIO.Partial (fromJust)

import qualified Data.UUID as UUID
import qualified RIO.HashMap as HashMap
import qualified RIO.Map as Map
import qualified RIO.Text as Text

type TableItem = HashMap Text AttributeValue
type AttributeItem = Map Text AttributeValue

logSource :: LogSource
logSource = "DynamoDB"

data DynamoDBException = forall e. Exception e => DynamoDBException e deriving Typeable
instance Exception DynamoDBException

instance Show DynamoDBException where
  show (DynamoDBException e) = "DynamoDBException (" ++ show e ++ ")"

data ValueError = ValueMissing (Maybe Text) | ValueInvalid (Maybe Text) AttributeValue (Maybe ValueError) deriving (Eq, Show)

instance Exception ValueError where
  toException = toException . DynamoDBException 
  fromException e = do
    DynamoDBException e' <- fromException e
    cast e'

instance Alternative (Either ValueError) where
  empty = Left (ValueMissing Nothing)
  Left _ <|> Right b = Right b
  a      <|> _ = a

class FromAttributeValue a where
  fromAttributeValue :: Maybe AttributeValue -> Either ValueError a

instance FromAttributeValue Text where
  fromAttributeValue (Just (S text)) = return text
  fromAttributeValue Nothing = throwError $ ValueMissing Nothing
  fromAttributeValue (Just errorValue) = throwError $ ValueInvalid Nothing errorValue Nothing

instance FromAttributeValue String where
  fromAttributeValue value = Text.unpack <$> fromAttributeValue value

instance FromAttributeValue ByteString where
  fromAttributeValue (Just (B (Base64 b))) = return b
  fromAttributeValue Nothing = throwError $ ValueMissing Nothing
  fromAttributeValue (Just errorValue) = throwError $ ValueInvalid Nothing errorValue Nothing

instance FromAttributeValue Int where
  fromAttributeValue (Just (N int)) = case fromText int of
    Just i -> return i
    Nothing -> throwError $ ValueInvalid Nothing (N int) Nothing
  fromAttributeValue Nothing = throwError $ ValueMissing Nothing
  fromAttributeValue (Just errorValue) = throwError $ ValueInvalid Nothing errorValue Nothing

instance FromAttributeValue Integer where 
  fromAttributeValue (Just (N int)) = case fromText int of
    Just i -> return i
    Nothing -> throwError $ ValueInvalid Nothing (N int) Nothing
  fromAttributeValue Nothing = throwError $ ValueMissing Nothing
  fromAttributeValue (Just errorValue) = throwError $ ValueInvalid Nothing errorValue Nothing

instance FromAttributeValue UUID where
  fromAttributeValue (Just (S text)) = case UUID.fromText text of
    Just uuid -> return uuid
    Nothing -> throwError $ ValueInvalid Nothing (S text) Nothing
  fromAttributeValue Nothing = throwError $ ValueMissing Nothing
  fromAttributeValue (Just errorValue) = throwError $ ValueInvalid Nothing errorValue Nothing

instance FromAttributeValue a => FromAttributeValue (Maybe a) where
  fromAttributeValue (Just NULL) = return Nothing 
  fromAttributeValue Nothing = return Nothing
  fromAttributeValue attr = Just <$> fromAttributeValue attr 

class FromTableItem a where
  fromTableItem :: TableItem -> Either ValueError a

readAttributeValue :: FromAttributeValue a => (Text -> i -> Maybe AttributeValue) -> i -> Text -> Either ValueError a
readAttributeValue lookup item key = case fromAttributeValue value of 
  Left (ValueMissing _) -> Left (ValueMissing (Just key))
  Left (ValueInvalid Nothing errorValue e) -> Left (ValueInvalid (Just key) errorValue e)
  Left e -> Left (ValueInvalid (Just key) (fromJust value) (Just e)) 
  Right a -> return a
  where
    value = lookup key item

readTableItemAttributeValue :: FromAttributeValue a => TableItem -> Text -> Either ValueError a
readTableItemAttributeValue = readAttributeValue HashMap.lookup

-- | Used to construct a type from a @TableItem@. This is best used to implement @FromTableItem@, e.g.,
--
-- > data User = User !Text !Int
-- > instance FromTableItem User where 
-- >   fromTableItem = readTableItem $ \read -> User <$> read "name" <*> read "age"
readTableItem :: ((forall v. FromAttributeValue v => Text -> Either ValueError v) -> Either ValueError a) -> TableItem -> Either ValueError a
readTableItem read item = read $ readTableItemAttributeValue item

readAttributeItemAttributeValue :: FromAttributeValue a => AttributeItem -> Text -> Either ValueError a
readAttributeItemAttributeValue = readAttributeValue Map.lookup 

-- | Used to read a type from an @AttributeValue@ of type @M@. This is best used to implement @FromAttributeValue@ for a complex type.
--
-- > data User = User !Text !Int
-- > instance FromAttributeValue User where
-- >   fromAttributeValue = readAttributeItem $ \read -> User <$> read "name" <*> read "age"
--
-- This is used for nested complex types.
readAttributeItem :: ((forall v. FromAttributeValue v => Text -> Either ValueError v) -> Either ValueError a) -> Maybe AttributeValue -> Either ValueError a
readAttributeItem read (Just (M item)) = case read $ readAttributeItemAttributeValue item of
  Left e -> throwError $ ValueInvalid Nothing (M item) (Just e) 
  Right a -> return a
readAttributeItem _ Nothing = throwError $ ValueMissing Nothing
readAttributeItem _ (Just errorValue) = throwError $ ValueInvalid Nothing errorValue Nothing

class ToAttributeValue a where
  toAttributeValue :: a -> AttributeValue

instance ToAttributeValue Text where
  toAttributeValue = S

instance ToAttributeValue String where
  toAttributeValue = S . Text.pack

instance ToAttributeValue ByteString where
  toAttributeValue = B . Base64

instance ToAttributeValue Int where
  toAttributeValue = N . Text.pack . show

instance ToAttributeValue Integer where
  toAttributeValue = N . Text.pack . show

instance ToAttributeValue UUID where
  toAttributeValue = S . UUID.toText

instance ToAttributeValue a => ToAttributeValue (Maybe a) where
  toAttributeValue (Just a) = toAttributeValue a
  toAttributeValue Nothing = NULL

class ToTableItem a where
  toTableItem :: a -> TableItem

isNotNull :: AttributeValue -> Bool
isNotNull NULL = False
isNotNull _ = True

-- | Used to write a type into a @TableItem@. Best used to implement @ToTableItem@.
--
-- > data User = User !Text !Int
-- > instance ToTableItem User where
-- >   toTableItem (User name age) = writeTableItem [ "name" =: name, "age" =: age ]
writeTableItem :: [(Text, AttributeValue)] -> TableItem 
writeTableItem = HashMap.filter isNotNull . HashMap.fromList

-- | Used to write a type into an @AttributeValue@ of type @M@ (Map).
-- Best used to implement @ToAttributeValue@ for a complex type.
-- See also @=:@.
--
-- > data User = User !Text !Int
-- > instance ToAttributeValue User where
-- >  toAttributeValue (User name age) = writeAttributeItem [ "name" =: name, "age" =: age ]
writeAttributeItem :: [(Text, AttributeValue)] -> AttributeValue 
writeAttributeItem = M . Map.filter isNotNull . Map.fromList

infixr 8 =:

-- | A helper to create a pair of key and @AttributeValue@.
--
-- `"x" =: 3` results in `("x", N "3")`.
(=:) :: ToAttributeValue a => Text -> a -> (Text, AttributeValue)
key =: value = (key, toAttributeValue value)

putItem' :: (HasAWSEnv env, MonadReader env m, MonadUnliftIO m, MonadThrow m) => Text -> TableItem -> m ()
putItem' table item = do
  let request = newPutItem table & (field @"item") .~ item
  void $ sendAWSThrowOnStatus request

putItem :: (ToTableItem a, HasAWSEnv env, MonadReader env m, MonadUnliftIO m, MonadThrow m) => Text -> a -> m ()
putItem table = putItem' table . toTableItem

getItem' :: (FromTableItem a, HasAWSEnv env, MonadReader env m, MonadUnliftIO m, MonadThrow m) => Text -> TableItem -> m (Maybe a) 
getItem' table item = do
  let request = newGetItem table & (field @"key") .~ item  
  response <- sendAWSThrowOnStatus request
  case fromTableItem <$> response^.(field @"item") of
    Just (Right a) -> return $ Just a
    Nothing -> return Nothing
    Just (Left e) -> throwM e

getItem 
  :: (ToTableItem k, FromTableItem a, HasAWSEnv env, MonadReader env m, MonadUnliftIO m, MonadThrow m)
  => Text -> k -> m (Maybe a) 
getItem table = getItem' table . toTableItem

scanWithFilter
  :: (FromTableItem a, HasAWSEnv env, HasLogFunc env, MonadReader env m, MonadUnliftIO m, MonadThrow m)
  => Text -> Maybe Text -> m [a]
scanWithFilter table filter = do
  let request = newScan table & (field @"filterExpression") .~ filter
  response <- sendAWSThrowOnStatus request
  case partitionEithers . map fromTableItem <$> response^.(field @"items") of
    Just (errors, items) -> do
      forM_ errors (logErrorS logSource . displayShow)
      return items
    Nothing -> return [] 

scan :: (FromTableItem a, HasAWSEnv env, HasLogFunc env, MonadReader env m, MonadUnliftIO m, MonadThrow m) => Text -> m [a]
scan table = scanWithFilter table Nothing

