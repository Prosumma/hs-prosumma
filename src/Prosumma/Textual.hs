module Prosumma.Textual (
  fromFieldTextual,
  fromStringTextual,
  parseJSONTextual,
  parseUrlPieceTextual,
  parseTextual,
  readPrec,
  readTextual,
  showTextual,
  toFieldTextual,
  toJSONTextual,
  unsafeFromText,
  FromText(..),
  ToText(..),
) where

import Data.Aeson.Types
import Data.Attoparsec.Text (parseOnly, endOfInput)
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
import Text.Read (readPrec, readS_to_Prec, ReadPrec)

import qualified Data.Attoparsec.Text as Atto

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

-- | Helper to implement `FromText`'s `fromText` using an Attoparsec parser.
parseTextual :: Atto.Parser a -> Text -> Maybe a 
parseTextual parser source = hush $ parseOnly (parser <* endOfInput) source

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

-- | Implements `Read` in terms of `FromText`.
--
-- > instance Read Foo where
-- >   readPrec = readTextual
readTextual :: FromText a => ReadPrec a
readTextual = readS_to_Prec $ const $ \input -> do
  case fromText (pack input) of
    Just thing -> [(thing, "")]
    Nothing -> []

-- | Implements `parseUrlPiece` in terms of 'Textual'. 
parseUrlPieceTextual :: FromText a => Text -> Text -> Either Text a
parseUrlPieceTextual name text = maybeToEither err $ fromText text
  where
    err = convertString $ sformat ("'" % stext % "' is not a valid " % stext % ".") text name
