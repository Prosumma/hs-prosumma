{-# LANGUAGE DataKinds, ExistentialQuantification, FlexibleContexts, OverloadedLabels, RankNTypes, TypeApplications, TypeFamilies, UndecidableInstances #-}

{-|
Module: Prosumma.AWS.DynamoDB
Description: Helpers and serialization for AWS DynamoDB using Amazonka 2.0.

The simplest way to make a type serializable to an @AttributeValue@ is simply to conform directly
to the `ToAttributeValue` protocol. However, if your attribute is of type `S`, `N`, or `B`, you can
get automatic serialization to `SS`, `NS`, `BS` if you follow a slightly more complex path:

> type instance TypeAttributeConstructor MyType = 'ConstructorS -- This means it uses the S and SS constructors.
> instance ToAttributeConstructorType MyType where
>   toAttributeConstructorType = myTypeToText -- or whatever
> instance ToScalarAttributeValue MyType -- no implementation necessary
> instance ToAttributeValue MyType where
>   toAttributeValue = toScalarAttributeValue

A similar situation exists for deserialization:

> type instance TypeAttributeConstructor MyType = 'ConstructorS
> instance FromAttributeConstructorType MyType where
>   fromAttributeConstructorType myType = myTypeFromText -- or whatever
> instance FromScalarAttributeValue MyType -- no implementation necessary
> instance FromAttributeValue MyType where
>   fromAttributeValue = fromScalarAttributeValue

Implementing these conformances automatically gives you (de)serialization for `[MyType]`, `Set MyType` and `Vector MyType`.

We could in theory say:

> instance ToScalarAttributeValue a => ToAttributeValue a where
>   toAttributeValue = toScalarAttributeValue

But this causes problems with overlapping instances. A similar issue exists for @FromAttributeValue@.
-}
module Prosumma.AWS.DynamoDB (
  AttributeConstructorType,
  AttributeItem,
  AttributeConstructor(..),
  DynamoDBContext(..),
  FromAttributeConstructorType(..),
  FromAttributeValue(..),
  FromScalarAttributeValue(..),
  FromTableItem(..),
  FromVector(..),
  FromVectorAttributeValue(..),
  HasDynamoDBTable(..),
  TableItem,
  ToAttributeConstructorType(..),
  ToAttributeValue(..),
  ToScalarAttributeValue(..),
  ToTableItem(..),
  ToVector(..),
  ToVectorAttributeValue(..),
  TypeAttributeConstructor,
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
import Data.Generics.Labels ()
import Data.Either.Extra
import Data.Typeable (cast)
import Data.UUID (UUID)
import Prosumma.AWS
import Prosumma.Textual
import RIO
import RIO.Time (Day)
import RIO.Partial (fromJust)

import qualified Data.UUID as UUID
import qualified RIO.HashMap as HashMap
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Time as Time
import qualified RIO.Text as Text
import qualified RIO.Vector as Vector

data DynamoDBContext = DynamoDBContext {
  env :: Env,
  logFunc :: LogFunc,
  table :: Text
}

class HasDynamoDBTable a where
  getDynamoDBTable :: a -> Text

instance HasAWSEnv DynamoDBContext where
  getAWSEnv = env

instance HasLogFunc DynamoDBContext where
  logFuncL = lens logFunc $ \ctx logFunc -> ctx{logFunc}

instance HasDynamoDBTable DynamoDBContext where
  getDynamoDBTable = table

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

dayTimeFormat :: String
dayTimeFormat = "%Y-%m-%d"

instance Exception ValueError where
  toException = toException . DynamoDBException 
  fromException e = do
    DynamoDBException e' <- fromException e
    cast e'

instance Alternative (Either ValueError) where
  empty = Left (ValueMissing Nothing)
  Left _ <|> Right b = Right b
  a      <|> _ = a

data AttributeConstructor = ConstructorS | ConstructorN | ConstructorB

type family AttributeConstructorType (kind :: AttributeConstructor) where
  AttributeConstructorType 'ConstructorS = Text
  AttributeConstructorType 'ConstructorN = Text
  AttributeConstructorType 'ConstructorB = Base64

type family TypeAttributeConstructor a :: AttributeConstructor 
type instance TypeAttributeConstructor Text = 'ConstructorS
type instance TypeAttributeConstructor String = 'ConstructorS
type instance TypeAttributeConstructor UUID = 'ConstructorS
type instance TypeAttributeConstructor UTCTime = 'ConstructorS
type instance TypeAttributeConstructor Day = 'ConstructorS
type instance TypeAttributeConstructor Int = 'ConstructorN
type instance TypeAttributeConstructor Integer = 'ConstructorN
type instance TypeAttributeConstructor ByteString = 'ConstructorB

class FromAttributeConstructorType a where
  fromAttributeConstructorType :: AttributeConstructorType (TypeAttributeConstructor a) -> Maybe a

instance FromAttributeConstructorType Text where
  fromAttributeConstructorType = Just

instance FromAttributeConstructorType String where
  fromAttributeConstructorType = Just . Text.unpack

instance FromAttributeConstructorType UUID where
  fromAttributeConstructorType = UUID.fromText

instance FromAttributeConstructorType UTCTime where
  fromAttributeConstructorType = Time.parseTimeM True Time.defaultTimeLocale utcTimeFormat . Text.unpack

instance FromAttributeConstructorType Day where
  fromAttributeConstructorType = Time.parseTimeM True Time.defaultTimeLocale dayTimeFormat . Text.unpack

instance FromAttributeConstructorType Int where
  fromAttributeConstructorType = fromText

instance FromAttributeConstructorType Integer where
  fromAttributeConstructorType = fromText

instance FromAttributeConstructorType ByteString where
  fromAttributeConstructorType = Just . unBase64

class FromScalarAttributeConstructor (kind :: AttributeConstructor) where
  fromScalarAttributeConstructor :: Proxy kind -> AttributeValue -> Either ValueError (AttributeConstructorType kind)

instance FromScalarAttributeConstructor 'ConstructorS where
  fromScalarAttributeConstructor _ (S text) = return text
  fromScalarAttributeConstructor _ attr = throwError $ valueInvalid attr

instance FromScalarAttributeConstructor 'ConstructorN where
  fromScalarAttributeConstructor _ (N text) = return text
  fromScalarAttributeConstructor _ attr = throwError $ valueInvalid attr

instance FromScalarAttributeConstructor 'ConstructorB where
  fromScalarAttributeConstructor _ (B base64) = return base64 
  fromScalarAttributeConstructor _ attr = throwError $ valueInvalid attr

class FromVectorAttributeConstructor (kind :: AttributeConstructor) where
  fromVectorAttributeConstructor :: Proxy kind -> AttributeValue -> Either ValueError (Vector (AttributeConstructorType kind))

instance FromVectorAttributeConstructor 'ConstructorS where
  fromVectorAttributeConstructor _ (SS vector) = return vector
  fromVectorAttributeConstructor _ attr = throwError $ valueInvalid attr

instance FromVectorAttributeConstructor 'ConstructorN where
  fromVectorAttributeConstructor _ (NS vector) = return vector
  fromVectorAttributeConstructor _ attr = throwError $ valueInvalid attr

instance FromVectorAttributeConstructor 'ConstructorB where
  fromVectorAttributeConstructor _ (BS vector) = return vector
  fromVectorAttributeConstructor _ attr = throwError $ valueInvalid attr

class (FromAttributeConstructorType a, FromScalarAttributeConstructor (TypeAttributeConstructor a)) => FromScalarAttributeValue a where
  fromScalarAttributeValue :: Maybe AttributeValue -> Either ValueError a
  fromScalarAttributeValue (Just attr) = do
    let proxy = Proxy :: Proxy (TypeAttributeConstructor a) 
    value <- fromScalarAttributeConstructor proxy attr
    maybeToEither (valueInvalid attr) $ fromAttributeConstructorType value
  fromScalarAttributeValue Nothing = throwError valueMissing

instance FromScalarAttributeValue Text
instance FromScalarAttributeValue String
instance FromScalarAttributeValue UUID
instance FromScalarAttributeValue UTCTime
instance FromScalarAttributeValue Day
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

class (FromAttributeConstructorType e, FromVectorAttributeConstructor (TypeAttributeConstructor e), FromVector e a) => FromVectorAttributeValue e a | a -> e where
  fromVectorAttributeValue :: Maybe AttributeValue -> Either ValueError a
  fromVectorAttributeValue (Just attr) = do
    let proxy = Proxy :: Proxy (TypeAttributeConstructor e) 
    vector <- fromVectorAttributeConstructor proxy attr
    maybeToEither (valueInvalid attr) $ fromVector <$> Vector.mapM fromAttributeConstructorType vector
  fromVectorAttributeValue Nothing = return mempty

instance (FromAttributeConstructorType a, FromVectorAttributeConstructor (TypeAttributeConstructor a)) => FromVectorAttributeValue a [a]
instance (FromAttributeConstructorType a, FromVectorAttributeConstructor (TypeAttributeConstructor a)) => FromVectorAttributeValue a (Vector a) 
instance (Ord a, FromAttributeConstructorType a, FromVectorAttributeConstructor (TypeAttributeConstructor a)) => FromVectorAttributeValue a (Set a) 

class FromAttributeValue a where
  fromAttributeValue :: Maybe AttributeValue -> Either ValueError a

instance (FromAttributeConstructorType a, FromVectorAttributeConstructor (TypeAttributeConstructor a)) => FromAttributeValue [a] where
  fromAttributeValue = fromVectorAttributeValue

instance (FromAttributeConstructorType a, FromVectorAttributeConstructor (TypeAttributeConstructor a)) => FromAttributeValue (Vector a) where
  fromAttributeValue = fromVectorAttributeValue

instance (Ord a, FromAttributeConstructorType a, FromVectorAttributeConstructor (TypeAttributeConstructor a)) => FromAttributeValue (Set a) where
  fromAttributeValue = fromVectorAttributeValue

instance FromAttributeValue Text where
  fromAttributeValue = fromScalarAttributeValue 

instance FromAttributeValue String where
  fromAttributeValue = fromScalarAttributeValue

instance FromAttributeValue UUID where
  fromAttributeValue = fromScalarAttributeValue

instance FromAttributeValue UTCTime where
  fromAttributeValue = fromScalarAttributeValue

instance FromAttributeValue Day where
  fromAttributeValue = fromScalarAttributeValue

instance FromAttributeValue ByteString where
  fromAttributeValue = fromScalarAttributeValue

instance FromAttributeValue Int where
  fromAttributeValue = fromScalarAttributeValue

instance FromAttributeValue Integer where 
  fromAttributeValue = fromScalarAttributeValue

instance FromAttributeValue AttributeValue where
  fromAttributeValue (Just attr) = return attr 
  fromAttributeValue Nothing = throwError valueMissing

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
  toAttributeConstructorType :: a -> AttributeConstructorType (TypeAttributeConstructor a)

instance ToAttributeConstructorType Text where
  toAttributeConstructorType = id 

instance ToAttributeConstructorType String where
  toAttributeConstructorType = Text.pack

instance ToAttributeConstructorType UUID where
  toAttributeConstructorType = UUID.toText

instance ToAttributeConstructorType UTCTime where
  toAttributeConstructorType = Text.pack . Time.formatTime Time.defaultTimeLocale utcTimeFormat

instance ToAttributeConstructorType Day where
  toAttributeConstructorType = Text.pack . Time.formatTime Time.defaultTimeLocale dayTimeFormat

instance ToAttributeConstructorType Int where
  toAttributeConstructorType = toText 

instance ToAttributeConstructorType Integer where
  toAttributeConstructorType = toText 

instance ToAttributeConstructorType ByteString where
  toAttributeConstructorType = Base64

class ToScalarAttributeConstructor (kind :: AttributeConstructor) where
  toScalarAttributeConstructor :: Proxy kind -> AttributeConstructorType kind -> AttributeValue 

instance ToScalarAttributeConstructor 'ConstructorS where
  toScalarAttributeConstructor _ = S

instance ToScalarAttributeConstructor 'ConstructorN where
  toScalarAttributeConstructor _ = N

instance ToScalarAttributeConstructor 'ConstructorB where
  toScalarAttributeConstructor _ = B

class (ToAttributeConstructorType a, ToScalarAttributeConstructor (TypeAttributeConstructor a)) => ToScalarAttributeValue a where
  toScalarAttributeValue :: a -> AttributeValue
  toScalarAttributeValue = toScalarAttributeConstructor (Proxy :: Proxy (TypeAttributeConstructor a)) . toAttributeConstructorType

instance ToScalarAttributeValue Text
instance ToScalarAttributeValue String
instance ToScalarAttributeValue ByteString
instance ToScalarAttributeValue Int
instance ToScalarAttributeValue Integer
instance ToScalarAttributeValue UUID
instance ToScalarAttributeValue UTCTime
instance ToScalarAttributeValue Day

class ToVectorAttributeConstructor (kind :: AttributeConstructor) where
  toVectorAttributeConstructor :: Proxy kind -> Vector (AttributeConstructorType kind) -> AttributeValue

instance ToVectorAttributeConstructor 'ConstructorS where
  toVectorAttributeConstructor _ = SS

instance ToVectorAttributeConstructor 'ConstructorN where
  toVectorAttributeConstructor _ = NS

instance ToVectorAttributeConstructor 'ConstructorB where
  toVectorAttributeConstructor _ = BS

class ToVector e c | c -> e where
  toVector :: c -> Vector e 

instance ToVector a (Vector a) where
  toVector = id

instance ToVector a [a] where
  toVector = Vector.fromList

instance ToVector a (Set a) where
  toVector = Vector.fromList . toList

class (ToAttributeConstructorType e, ToVectorAttributeConstructor (TypeAttributeConstructor e), ToVector e a) => ToVectorAttributeValue e a | a -> e where
  toVectorAttributeValue :: a -> AttributeValue
  toVectorAttributeValue = toVectorAttributeConstructor (Proxy :: Proxy (TypeAttributeConstructor e)) . Vector.map toAttributeConstructorType . toVector

instance (ToAttributeConstructorType e, ToVectorAttributeConstructor (TypeAttributeConstructor e)) => ToVectorAttributeValue e (Vector e) 
instance (ToAttributeConstructorType e, ToVectorAttributeConstructor (TypeAttributeConstructor e)) => ToVectorAttributeValue e [e]
instance (ToAttributeConstructorType e, ToVectorAttributeConstructor (TypeAttributeConstructor e)) => ToVectorAttributeValue e (Set e) 

class ToAttributeValue a where
  toAttributeValue :: a -> AttributeValue

instance (ToAttributeConstructorType e, ToVectorAttributeConstructor (TypeAttributeConstructor e)) => ToAttributeValue (Vector e) where 
  toAttributeValue = toVectorAttributeValue

instance (ToAttributeConstructorType e, ToVectorAttributeConstructor (TypeAttributeConstructor e)) => ToAttributeValue [e] where 
  toAttributeValue = toVectorAttributeValue

instance (ToAttributeConstructorType e, ToVectorAttributeConstructor (TypeAttributeConstructor e)) => ToAttributeValue (Set e) where 
  toAttributeValue = toVectorAttributeValue

instance ToAttributeValue Text where
  toAttributeValue = toScalarAttributeValue 

instance ToAttributeValue String where
  toAttributeValue = toScalarAttributeValue 

instance ToAttributeValue ByteString where
  toAttributeValue = toScalarAttributeValue

instance ToAttributeValue Int where
  toAttributeValue = toScalarAttributeValue 

instance ToAttributeValue Integer where
  toAttributeValue = toScalarAttributeValue 

instance ToAttributeValue UUID where
  toAttributeValue = toScalarAttributeValue 

instance ToAttributeValue UTCTime where
  toAttributeValue = toScalarAttributeValue 

instance ToAttributeValue Day where
  toAttributeValue = toScalarAttributeValue 

instance ToAttributeValue AttributeValue where
  toAttributeValue = id

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
  let request = newPutItem table & #item .~ item
  void $ sendAWSThrowOnStatus request

putItem :: (ToTableItem a, HasAWSEnv env, MonadReader env m, MonadUnliftIO m, MonadThrow m) => Text -> a -> m ()
putItem table = putItem' table . toTableItem

getItem' :: (FromTableItem a, HasAWSEnv env, MonadReader env m, MonadUnliftIO m, MonadThrow m) => Text -> TableItem -> m (Maybe a) 
getItem' table item = do
  let request = newGetItem table & #key .~ item  
  response <- sendAWSThrowOnStatus request
  case fromTableItem <$> response ^. #item of
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
  let request = newScan table & #filterExpression .~ filter
  response <- sendAWSThrowOnStatus request
  case partitionEithers . map fromTableItem <$> response ^. #items of
    Just (errors, items) -> do
      for_ errors (logErrorS logSource . displayShow)
      return items
    Nothing -> return [] 

scan :: (FromTableItem a, HasAWSEnv env, HasLogFunc env, MonadReader env m, MonadUnliftIO m, MonadThrow m) => Text -> m [a]
scan table = scanWithFilter table Nothing
