{-# LANGUAGE DataKinds, ExistentialQuantification, FlexibleContexts, RankNTypes, TypeApplications, TypeFamilies, UndecidableInstances #-}

module Prosumma.AWS.DynamoDB (
  AttributeConstructorType,
  AttributeItem,
  AttributeKind(..),
  FromAttributeConstructorType(..),
  FromAttributeValue(..),
  FromScalarAttributeConstructor(..),
  FromScalarAttributeValue(..),
  FromTableItem(..),
  FromVector(..),
  FromVectorAttributeConstructor(..),
  FromVectorAttributeValue(..),
  TableItem,
  ToAttributeConstructorType(..),
  ToAttributeValue(..),
  ToTableItem(..),
  ToVector(..),
  ToVectorAttributeConstructor(..),
  ToVectorAttributeValue(..),
  TypeAttributeKind,
  ValueError(..),
  getItem,
  getItem',
  putItem,
  putItem',
  readAttributeItem,
  readTableItem,
  scan,
  scanWithFilter,
  utcTimeFormat,
  valueInvalid,
  valueMissing,
  writeAttributeItem,
  writeTableItem,
  (=:)
) where

import Amazonka hiding (query)
import Amazonka.DynamoDB
import Control.Applicative
import Control.Monad.Error.Class
import Data.Generics.Product
import Data.Either.Extra
import Data.Typeable (cast)
import Data.UUID (UUID)
import Prosumma.AWS
import Prosumma.Textual
import RIO
import RIO.Partial (fromJust)

import qualified Data.UUID as UUID
import qualified RIO.HashMap as HashMap
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Time as Time
import qualified RIO.Text as Text
import qualified RIO.Vector as Vector

type TableItem = HashMap Text AttributeValue
type AttributeItem = Map Text AttributeValue

logSource :: LogSource
logSource = "DynamoDB"

data DynamoDBException = forall e. Exception e => DynamoDBException e deriving Typeable
instance Exception DynamoDBException

instance Show DynamoDBException where
  show (DynamoDBException e) = "DynamoDBException (" ++ show e ++ ")"

data ValueError = ValueMissing (Maybe Text) | ValueInvalid (Maybe Text) AttributeValue (Maybe ValueError) deriving (Eq, Show)

valueInvalid :: AttributeValue -> ValueError
valueInvalid attr = ValueInvalid Nothing attr Nothing

valueMissing :: ValueError
valueMissing = ValueMissing Nothing

utcTimeFormat :: String
utcTimeFormat = "%Y-%m-%dT%H:%M:%S"

instance Exception ValueError where
  toException = toException . DynamoDBException 
  fromException e = do
    DynamoDBException e' <- fromException e
    cast e'

instance Alternative (Either ValueError) where
  empty = Left (ValueMissing Nothing)
  Left _ <|> Right b = Right b
  a      <|> _ = a

data AttributeKind = KindS | KindN | KindB

type family AttributeConstructorType (kind :: AttributeKind) where
  AttributeConstructorType 'KindS = Text
  AttributeConstructorType 'KindN = Text
  AttributeConstructorType 'KindB = Base64

type family TypeAttributeKind a :: AttributeKind 
type instance TypeAttributeKind Text = 'KindS
type instance TypeAttributeKind String = 'KindS
type instance TypeAttributeKind UUID = 'KindS
type instance TypeAttributeKind UTCTime = 'KindS
type instance TypeAttributeKind Int = 'KindN
type instance TypeAttributeKind Integer = 'KindN
type instance TypeAttributeKind ByteString = 'KindB

class FromAttributeConstructorType a where
  fromAttributeConstructorType :: AttributeConstructorType (TypeAttributeKind a) -> Maybe a

instance FromAttributeConstructorType Text where
  fromAttributeConstructorType = Just

instance FromAttributeConstructorType String where
  fromAttributeConstructorType = Just . Text.unpack

instance FromAttributeConstructorType UUID where
  fromAttributeConstructorType = UUID.fromText

instance FromAttributeConstructorType UTCTime where
  fromAttributeConstructorType = Time.parseTimeM True Time.defaultTimeLocale utcTimeFormat . Text.unpack

instance FromAttributeConstructorType Int where
  fromAttributeConstructorType = fromText

instance FromAttributeConstructorType Integer where
  fromAttributeConstructorType = fromText

instance FromAttributeConstructorType ByteString where
  fromAttributeConstructorType = Just . unBase64

class FromScalarAttributeConstructor (kind :: AttributeKind) where
  fromScalarAttributeConstructor :: Proxy kind -> AttributeValue -> Either ValueError (AttributeConstructorType kind)

instance FromScalarAttributeConstructor 'KindS where
  fromScalarAttributeConstructor _ (S text) = return text
  fromScalarAttributeConstructor _ attr = throwError $ valueInvalid attr

instance FromScalarAttributeConstructor 'KindN where
  fromScalarAttributeConstructor _ (N text) = return text
  fromScalarAttributeConstructor _ attr = throwError $ valueInvalid attr

instance FromScalarAttributeConstructor 'KindB where
  fromScalarAttributeConstructor _ (B base64) = return base64 
  fromScalarAttributeConstructor _ attr = throwError $ valueInvalid attr

class FromVectorAttributeConstructor (kind :: AttributeKind) where
  fromVectorAttributeConstructor :: Proxy kind -> AttributeValue -> Either ValueError (Vector (AttributeConstructorType kind))

instance FromVectorAttributeConstructor 'KindS where
  fromVectorAttributeConstructor _ (SS vector) = return vector
  fromVectorAttributeConstructor _ attr = throwError $ valueInvalid attr

instance FromVectorAttributeConstructor 'KindN where
  fromVectorAttributeConstructor _ (NS vector) = return vector
  fromVectorAttributeConstructor _ attr = throwError $ valueInvalid attr

instance FromVectorAttributeConstructor 'KindB where
  fromVectorAttributeConstructor _ (BS vector) = return vector
  fromVectorAttributeConstructor _ attr = throwError $ valueInvalid attr

class (FromAttributeConstructorType a, FromScalarAttributeConstructor (TypeAttributeKind a)) => FromScalarAttributeValue a where
  fromScalarAttributeValue :: Maybe AttributeValue -> Either ValueError a
  fromScalarAttributeValue (Just attr) = do
    let proxy = Proxy :: Proxy (TypeAttributeKind a) 
    value <- fromScalarAttributeConstructor proxy attr
    maybeToEither (valueInvalid attr) $ fromAttributeConstructorType value
  fromScalarAttributeValue Nothing = throwError valueMissing

instance FromScalarAttributeValue Text
instance FromScalarAttributeValue String
instance FromScalarAttributeValue UUID
instance FromScalarAttributeValue UTCTime
instance FromScalarAttributeValue Int
instance FromScalarAttributeValue Integer
instance FromScalarAttributeValue ByteString

class Monoid c => FromVector e c | c -> e where
  fromVector :: Vector e -> c

instance FromVector a (Vector a) where
  fromVector = id

instance FromVector a [a] where
  fromVector = toList

instance Ord a => FromVector a (Set a) where
  fromVector = Set.fromList . toList

class (FromAttributeConstructorType e, FromVectorAttributeConstructor (TypeAttributeKind e), FromVector e a) => FromVectorAttributeValue e a | a -> e where
  fromVectorAttributeValue :: Maybe AttributeValue -> Either ValueError a
  fromVectorAttributeValue (Just attr) = do
    let proxy = Proxy :: Proxy (TypeAttributeKind e) 
    vector <- fromVectorAttributeConstructor proxy attr
    maybeToEither (valueInvalid attr) $ fromVector <$> Vector.mapM fromAttributeConstructorType vector
  fromVectorAttributeValue Nothing = return mempty

instance (FromAttributeConstructorType a, FromVectorAttributeConstructor (TypeAttributeKind a)) => FromVectorAttributeValue a [a]
instance (FromAttributeConstructorType a, FromVectorAttributeConstructor (TypeAttributeKind a)) => FromVectorAttributeValue a (Vector a) 
instance (Ord a, FromAttributeConstructorType a, FromVectorAttributeConstructor (TypeAttributeKind a)) => FromVectorAttributeValue a (Set a) 

class FromAttributeValue a where
  fromAttributeValue :: Maybe AttributeValue -> Either ValueError a

instance (FromAttributeConstructorType a, FromVectorAttributeConstructor (TypeAttributeKind a)) => FromAttributeValue [a] where
  fromAttributeValue = fromVectorAttributeValue

instance (FromAttributeConstructorType a, FromVectorAttributeConstructor (TypeAttributeKind a)) => FromAttributeValue (Vector a) where
  fromAttributeValue = fromVectorAttributeValue

instance (Ord a, FromAttributeConstructorType a, FromVectorAttributeConstructor (TypeAttributeKind a)) => FromAttributeValue (Set a) where
  fromAttributeValue = fromVectorAttributeValue

instance FromAttributeValue Text where
  fromAttributeValue = fromScalarAttributeValue 

instance FromAttributeValue String where
  fromAttributeValue = fromScalarAttributeValue

instance FromAttributeValue UUID where
  fromAttributeValue = fromScalarAttributeValue

instance FromAttributeValue UTCTime where
  fromAttributeValue = fromScalarAttributeValue

instance FromAttributeValue ByteString where
  fromAttributeValue = fromScalarAttributeValue

instance FromAttributeValue Int where
  fromAttributeValue = fromScalarAttributeValue

instance FromAttributeValue Integer where 
  fromAttributeValue = fromScalarAttributeValue

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

class ToAttributeConstructorType a where
  toAttributeConstructorType :: a -> AttributeConstructorType (TypeAttributeKind a)

instance ToAttributeConstructorType Text where
  toAttributeConstructorType = id 

instance ToAttributeConstructorType String where
  toAttributeConstructorType = Text.pack

instance ToAttributeConstructorType UUID where
  toAttributeConstructorType = UUID.toText

instance ToAttributeConstructorType UTCTime where
  toAttributeConstructorType = Text.pack . Time.formatTime Time.defaultTimeLocale utcTimeFormat

instance ToAttributeConstructorType Int where
  toAttributeConstructorType = toText 

instance ToAttributeConstructorType Integer where
  toAttributeConstructorType = toText 

instance ToAttributeConstructorType ByteString where
  toAttributeConstructorType = Base64

class ToVectorAttributeConstructor (kind :: AttributeKind) where
  toVectorAttributeConstructor :: Proxy kind -> Vector (AttributeConstructorType kind) -> AttributeValue

instance ToVectorAttributeConstructor 'KindS where
  toVectorAttributeConstructor _ = SS

instance ToVectorAttributeConstructor 'KindN where
  toVectorAttributeConstructor _ = NS

instance ToVectorAttributeConstructor 'KindB where
  toVectorAttributeConstructor _ = BS

class ToVector e c | c -> e where
  toVector :: c -> Vector e 

instance ToVector a (Vector a) where
  toVector = id

instance ToVector a [a] where
  toVector = Vector.fromList

instance ToVector a (Set a) where
  toVector = Vector.fromList . toList

class (ToAttributeConstructorType e, ToVectorAttributeConstructor (TypeAttributeKind e), ToVector e a) => ToVectorAttributeValue e a | a -> e where
  toVectorAttributeValue :: a -> AttributeValue
  toVectorAttributeValue = toVectorAttributeConstructor (Proxy :: Proxy (TypeAttributeKind e)) . Vector.map toAttributeConstructorType . toVector

instance (ToAttributeConstructorType e, ToVectorAttributeConstructor (TypeAttributeKind e)) => ToVectorAttributeValue e (Vector e) 
instance (ToAttributeConstructorType e, ToVectorAttributeConstructor (TypeAttributeKind e)) => ToVectorAttributeValue e [e]
instance (ToAttributeConstructorType e, ToVectorAttributeConstructor (TypeAttributeKind e)) => ToVectorAttributeValue e (Set e) 

class ToAttributeValue a where
  toAttributeValue :: a -> AttributeValue

instance (ToAttributeConstructorType e, ToVectorAttributeConstructor (TypeAttributeKind e)) => ToAttributeValue (Vector e) where 
  toAttributeValue = toVectorAttributeValue

instance (ToAttributeConstructorType e, ToVectorAttributeConstructor (TypeAttributeKind e)) => ToAttributeValue [e] where 
  toAttributeValue = toVectorAttributeValue

instance (ToAttributeConstructorType e, ToVectorAttributeConstructor (TypeAttributeKind e)) => ToAttributeValue (Set e) where 
  toAttributeValue = toVectorAttributeValue

instance ToAttributeValue Text where
  toAttributeValue = S . toAttributeConstructorType

instance ToAttributeValue String where
  toAttributeValue = S . toAttributeConstructorType 

instance ToAttributeValue ByteString where
  toAttributeValue = B . toAttributeConstructorType 

instance ToAttributeValue Int where
  toAttributeValue = N . toAttributeConstructorType 

instance ToAttributeValue Integer where
  toAttributeValue = N . toAttributeConstructorType 

instance ToAttributeValue UUID where
  toAttributeValue = S . toAttributeConstructorType 

instance ToAttributeValue UTCTime where
  toAttributeValue = S . toAttributeConstructorType

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

