{-# LANGUAGE ViewPatterns #-}

module Prosumma.Aeson (
 JSONStripPredicate,
 ParentContext(..),
 StripIn(..),
 ofAll,
 stripInArrays,
 stripInObjects,
 ofNullArrays,
 ofEmptyStrings,
 ofFalse,
 stripJSON,
 ofNulls,
 ofNullObjects,
 ofNullStrings,
 (?.=)
) where

import Data.Aeson
import Data.Aeson.Types
import Data.Char
import RIO

import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import qualified RIO.Text as T

infixr 8 ?.=

-- | If the second parameter is null, returns an empty list.
--
-- This is chiefly useful for `Maybe`. For example:
--
-- > "badge" ?.= badge
--
-- If `badge` is `Maybe Int`, we'll get a `[Pair]` if it's something
-- and `[]` if it's nothing.
--
-- Prefer @ofNullsFields@ to this. 
(?.=) :: (Foldable t, ToJSON (t v)) => Key -> t v -> [Pair]
key ?.= value
  | null value = []
  | otherwise = [key .= value]

data ParentContext = NoParent | ObjectParent Key ParentContext | ArrayParent Int ParentContext deriving (Eq, Show)

type JSONStripPredicate = ParentContext -> Value -> Bool

foldMapWithIndex :: Monoid m => (Int -> a -> m) -> Vector a -> m
foldMapWithIndex f v = V.foldr fold mempty $ V.imap (,) v
  where
    fold (i, a) m = f i a <> m

stripJSON' :: ParentContext -> JSONStripPredicate -> Value -> Value
stripJSON' parent shouldStrip (Object o) = Object $ KM.foldMapWithKey strip o
  where
    strip k c = let
        context = ObjectParent k parent
        -- Strip the children of this node, if possible
        stripped = stripJSON' context shouldStrip c
        -- Once the children have been stripped, we check to see whether
        -- this node can be stripped from its parent
      in if shouldStrip context stripped then mempty else KM.singleton k stripped
stripJSON' parent shouldStrip (Array a) = Array $ foldMapWithIndex strip a
  where
    strip i c = let
        context = ArrayParent i parent
        -- Strip the children of this node, if possible
        stripped = stripJSON' context shouldStrip c
        -- Once the children have been stripped, we check to see whether
        -- this node can be stripped from its parent
      in if shouldStrip context stripped then mempty else V.singleton stripped
stripJSON' _ _ j = j

-- | Strips JSON of extraneous data and compacts it.
--
-- "Stripping" is the act of removing a node from its parent.
-- We can strip nodes based on any criteria, but typically
-- we want to strip null fields from objects. This can be
-- achieved quite easily:
--
-- > stripJSON (ofNulls InObjects)
--
-- But if we want to get more aggressive, we can strip
-- empty objects, empty arrays, empty strings, `false`, etc.
--
-- > stripJSON (ofAll InBoth)
--
-- The above will take the JSON 
-- `{"foo": null, "bar": [null, {}]}`
-- and turn it into 
-- `{}`.
-- 
-- It's also possible to roll your own strip predicate.
-- For example, if you always want to include nodes with
-- the key "data", you can say:
--
-- > keepData :: JSONStripPredicate
-- > keepData (ObjectParent "data" _) _ = False
-- > keepData parent value = ofAll InObjects parent value
--
-- This can then be passed when stripping:
--
-- > stripJSON keepData
stripJSON :: JSONStripPredicate -> Value -> Value
stripJSON = stripJSON' NoParent

type ShouldStrip = Value -> Bool

whenNull :: ShouldStrip
whenNull Null = True
whenNull _ = False

whenNullString :: ShouldStrip
whenNullString (String s) = T.null s
whenNullString _ = False

whenEmptyString :: ShouldStrip
whenEmptyString (String s) = T.null s || T.all isSpace s
whenEmptyString _ = False

whenNullArray :: ShouldStrip
whenNullArray (Array a) = null a
whenNullArray _ = False

whenNullObject :: ShouldStrip
whenNullObject (Object o) = null o
whenNullObject _ = False

whenFalse :: ShouldStrip
whenFalse (Bool b) = not b
whenFalse _ = False

data StripIn = InArrays | InObjects | InBoth deriving (Eq, Show)

stripInArrays :: StripIn -> Bool
stripInArrays InArrays = True
stripInArrays InBoth = True
stripInArrays InObjects = False

stripInObjects :: StripIn -> Bool
stripInObjects InObjects = True
stripInObjects InBoth = True
stripInObjects InArrays = False

strip :: ShouldStrip -> StripIn -> JSONStripPredicate
strip shouldStrip (stripInArrays -> True) (ArrayParent _ _) value = shouldStrip value
strip shouldStrip (stripInObjects -> True) (ObjectParent _ _) value = shouldStrip value
strip _ _ _ _ = False

ofNulls :: StripIn -> JSONStripPredicate
ofNulls = strip whenNull

ofNullStrings :: StripIn -> JSONStripPredicate
ofNullStrings = strip whenNullString

ofEmptyStrings :: StripIn -> JSONStripPredicate
ofEmptyStrings = strip whenEmptyString

ofNullArrays :: StripIn -> JSONStripPredicate
ofNullArrays = strip whenNullArray

ofNullObjects :: StripIn -> JSONStripPredicate
ofNullObjects = strip whenNullObject

ofFalse :: StripIn -> JSONStripPredicate
ofFalse = strip whenFalse

ofAll :: StripIn -> JSONStripPredicate
ofAll stripIn = ofNulls stripIn <||> ofEmptyStrings stripIn <||> ofNullArrays stripIn <||> ofNullObjects stripIn <||> ofFalse stripIn

infixr 1 <||>

orJSONStripPredicate :: JSONStripPredicate -> JSONStripPredicate -> JSONStripPredicate
orJSONStripPredicate a b parent value = a parent value || b parent value

(<||>) :: JSONStripPredicate -> JSONStripPredicate -> JSONStripPredicate
(<||>) = orJSONStripPredicate
