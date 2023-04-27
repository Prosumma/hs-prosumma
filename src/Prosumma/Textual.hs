module Prosumma.Textual (
  Textual(..),
  ifMatchTextual,
  fromFieldTextual,
  fromStringTextual,
  parseJSONTextual,
  unsafeFromText,
  showTextual
) where

import Data.Aeson.Types
import Database.PostgreSQL.Simple.FromField
import Data.String.Conversions
import Data.Text.Read
import Prosumma.Util
import RIO
import RIO.Partial
import Text.Regex.TDFA
import Text.Printf
import RIO.Text

class Textual a where
  fromText :: Text -> Maybe a
  toText :: a -> Text

instance Textual Text where
  fromText = Just 
  toText = id

instance Textual Integer where
  fromText text = fst <$> maybeFromRight (decimal text)
  toText = pack . show

instance Textual Int where
  fromText text = fst <$> maybeFromRight (decimal text)
  toText = pack . show

-- | Unsafely converts from `Text` to a `Textual`.
--
-- Causes a runtime error if conversion is invalid.
-- Use this only if you're very sure that the `Text` 
-- is valid for the target `Textual`.
unsafeFromText :: Textual a => Text -> a
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
fromFieldTextual :: (Textual a, Typeable a) => String -> FieldParser a
fromFieldTextual name field mdata = do
  text <- fromField field mdata
  case fromText text of
    Just value -> return value
    Nothing -> returnError ConversionFailed field $ printf "'%s' is not a valid %s." text name 

-- | Helper to parse from JSON to a `Textual` instance.
--
-- > instance FromJSON Foo where
-- >  parseJSON = parseJSONTextual "Foo"
parseJSONTextual :: Textual a => String -> Value -> Parser a
parseJSONTextual name (String text) = case fromText text of
  Just thing -> return thing
  Nothing -> fail $ printf "'%s' is not a valid %s." text name
parseJSONTextual name invalid = typeMismatch name invalid

-- | Helper to implement `IsString`'s `fromString` for a `Textual`.
--
-- > instance IsString Foo where
-- >   fromString = fromStringTextual "Foo"
fromStringTextual :: Textual a => String -> String -> a
fromStringTextual name string = case fromText (fromString string) of
  Just thing -> thing
  Nothing -> error $ printf "'%s' is not a valid %s." string name

-- | Implements `Show` in terms of `Textual`.
--
-- > instance Show Foo where
-- >   show = showTextual
showTextual :: Textual a => a -> String
showTextual = convertString . toText
