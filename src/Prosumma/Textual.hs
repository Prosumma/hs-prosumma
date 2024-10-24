module Prosumma.Textual (
  fromFieldTextual,
  fromStringTextual,
  ifMatchTextual,
  parseJSONTextual,
  parseUrlPieceTextual,
  showTextual,
  toFieldTextual,
  toJSONTextual,
  unsafeFromText,
  FromText(..),
  ToText(..),
) where

import Data.Aeson.Types
import Data.Either.Extra
import Data.String.Conversions
import Data.Text.Read
import Database.PostgreSQL.Simple.FromField hiding (format)
import Database.PostgreSQL.Simple.ToField
import Formatting
import Prosumma.Util
import RIO hiding (Reader)
import RIO.Partial
import RIO.Text
import Text.Printf
import Text.Regex.TDFA

class FromText a where
  fromText :: Text -> Maybe a
  
class ToText a where
  toText :: a -> Text

instance FromText Text where
  fromText = Just
  
instance ToText Text where
  toText = id

instance FromText ByteString where
  fromText = Just . convertString

instance ToText ByteString where
  toText = convertString
  
instance FromText Int where
  fromText = fromTextReader decimal
  
instance ToText Int where
  toText = pack . show

instance FromText Integer where
  fromText = fromTextReader decimal
  
instance ToText Integer where
  toText = pack . show

instance FromText Double where
  fromText = fromTextReader double

instance ToText Double where
  toText = pack . show

-- | Unsafely converts from `Text` to a `Textual`.
--
-- Causes a runtime error if conversion is invalid.
-- Use this only if you're very sure that the `Text` 
-- is valid for the target `Textual`.
unsafeFromText :: FromText a => Text -> a
unsafeFromText = fromJust . fromText

-- | Helper to implement `Textual`'s `fromText`
-- for simple newtype wrappers.
--
-- > instance Textual Foo where
-- >   fromText = ifMatchTextual "^[a-z]{3,7}$" Foo
-- >   toText (Foo foo) = foo
ifMatchTextual :: Text -> (Text -> a) -> Text -> Maybe a 
ifMatchTextual regex make source = if source =~ regex
  then Just $ make source
  else Nothing

-- | Helper to implement `FromField`'s `fromField` for a `Textual`.
--
-- > instance FromField Foo where
-- >  fromField = fromFieldTextual "Foo"
fromFieldTextual :: (FromText a, Typeable a) => String -> FieldParser a
fromFieldTextual name field mdata = do
  text <- fromField field mdata
  case fromText text of
    Just value -> return value
    Nothing -> returnError ConversionFailed field $ printf "'%s' is not a valid %s." text name 

-- | Helper to implement `ToField`'s `toField` for a `Textual`.
toFieldTextual :: ToText a => a -> Action
toFieldTextual = toField . toText

-- | Helper to parse from JSON to a `Textual` instance.
--
-- > instance FromJSON Foo where
-- >  parseJSON = parseJSONTextual "Foo"
parseJSONTextual :: FromText a => String -> Value -> Parser a
parseJSONTextual name (String text) = case fromText text of
  Just thing -> return thing
  Nothing -> fail $ printf "'%s' is not a valid %s." text name
parseJSONTextual name invalid = typeMismatch name invalid

-- | Helper to convert a `Textual` to JSON.
toJSONTextual :: ToText a => a -> Value 
toJSONTextual = toJSON . toText 

-- | Helper to implement `IsString`'s `fromString` for a `Textual`.
--
-- > instance IsString Foo where
-- >   fromString = fromStringTextual "Foo"
fromStringTextual :: FromText a => String -> String -> a
fromStringTextual name string = case fromText (fromString string) of
  Just thing -> thing
  Nothing -> error $ printf "'%s' is not a valid %s." string name

-- | Implements `Show` in terms of `Textual`.
--
-- > instance Show Foo where
-- >   show = showTextual
showTextual :: ToText a => a -> String
showTextual = convertString . toText

-- | Implements `parseUrlPiece` in terms of 'Textual'. 
parseUrlPieceTextual :: FromText a => Text -> Text -> Either Text a
parseUrlPieceTextual name text = maybeToEither err $ fromText text
  where
    err = convertString $ format ("'" % stext % "' is not a valid " % stext % ".") text name
