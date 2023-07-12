{-# LANGUAGE DataKinds, RankNTypes, TypeApplications #-}

module Prosumma.AWS.DynamoDB (
  AttributeValue,
  lookupAttributeValue,
  readErrorKeyNotFound,
  readErrorIncorrectType,
  readTableItem,
  readTableItemWithIndex,
  scanTable,
  ReadAttributeValue(..),
  TableItem,
  TableReadException(..),
  TableScanException(..)
) where

import Amazonka.DynamoDB
import Data.Either.Extra
import Data.Generics.Product
import Prosumma.AWS
import Prosumma.Textual
import RIO
import Text.Printf

import qualified RIO.HashMap as HM

type TableItem = HashMap Text AttributeValue

class ReadAttributeValue a where
  readAttributeValue :: AttributeValue -> Maybe a

instance ReadAttributeValue Text where
  readAttributeValue (S text) = Just text
  readAttributeValue _other = Nothing

instance ReadAttributeValue ByteString where
  readAttributeValue = readAttributeValue >=> fromText 

instance ReadAttributeValue Integer where
  readAttributeValue (N integer) = fromText integer 
  readAttributeValue (S text) = fromText text 
  readAttributeValue _other = Nothing

instance ReadAttributeValue Int where
  readAttributeValue (N integer) = fromText integer 
  readAttributeValue (S text) = fromText text 
  readAttributeValue _other = Nothing

instance ReadAttributeValue Bool where
  readAttributeValue (BOOL bool) = Just bool
  readAttributeValue _other = Nothing

instance ReadAttributeValue s => ReadAttributeValue (Maybe s) where
  readAttributeValue = Just . readAttributeValue 

readErrorKeyNotFound :: String
readErrorKeyNotFound = "The key '%s' was not found."

readErrorIncorrectType :: String
readErrorIncorrectType = "The key '%s' was found but was not the correct type."

lookupAttributeValue :: (ReadAttributeValue v) => TableItem -> Text -> Either String v
lookupAttributeValue item key = maybeToEither keyNotFound read >>= maybeToEither incorrectType . readAttributeValue
  where
    read = HM.lookup key item 
    keyNotFound = printf readErrorKeyNotFound key
    incorrectType = printf readErrorIncorrectType key

-- | Reads a @TableItem@ into another type.
--
-- A simple example will suffice:
--
-- > data Watusi = Watusi { watusiFoo :: Text, watusiBar :: Text } deriving Show
-- >
-- > readWatusi :: TableItem -> Either String Watusi
-- > readWatusi = readTableItem $ \read -> Watusi <$> read "Foo" <*> read "Bar"
--
-- This can then be used to read multiple items. Two recommendations depending upon
-- what you want.
--
-- > readWatusis :: Traversable t => t TableItem -> [Either String Watusi] 
-- > readWatusis = map readWatusi
--
-- or
--
-- > readWatusis :: Traversable t => t TableItem -> Either String [Watusi]
-- > readWatusis = mapM readWatusi
readTableItem :: ((forall v. ReadAttributeValue v => Text -> Either String v) -> Either String a) -> TableItem -> Either String a
readTableItem read item = read $ lookupAttributeValue item

-- | Reads a @TableItem@ into another type.
--
-- This is for use when reading multiple rows. It can pinpoint the row on which the error
-- occurred. It is the form expected by @scanTable@.
--
-- A simple example will suffice:
--
-- > data Watusi = Watusi { watusiFoo :: Text, watusiBar :: Text } deriving Show
-- >
-- > readWatusiWithIndex :: Int -> TableItem -> Either String Watusi
-- > readWatusiWithIndex = readTableItemWithIndex $ \read -> Watusi <$> read "Foo" <*> read "Bar"
readTableItemWithIndex :: ((forall v. ReadAttributeValue v => Text -> Either String v) -> Either String a) -> Int -> TableItem -> Either String a
readTableItemWithIndex read index item = case readTableItem read item of
  Right a -> Right a
  Left e -> Left $ printf "At index %d: %s" index e

data TableScanException = TableScanException !Text !Int deriving (Show, Typeable)
instance Exception TableScanException

data TableReadException = TableReadException !Text !String deriving (Show, Typeable)
instance Exception TableReadException

-- | Helper function to scan a DynamoDB table into a list of some type. 
--
-- This is best used with @readTableItem@, e.g.,
--
-- > newtype Foo = Foo !Text
-- >
-- > scanFoo :: RIO [a]  
-- > scanFoo = scanTable "Foo" $ readTableItem $ \read -> Foo <$> read "Foo" 
scanTable :: (HasAWSEnv env, MonadReader env m, MonadUnliftIO m, MonadThrow m) => Text -> (Int -> TableItem -> Either String a) -> m [a] 
scanTable table read = do
  response <- sendAWSThrowOnError (TableScanException table) $ newScan table
  case response ^. (field @"items") of
    Just items -> case zipWithM read [0..] items of
      Right records -> return records
      Left e -> throwIO $ TableReadException table e 
    Nothing -> return []