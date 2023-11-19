module Prosumma.Aeson (
  Pairs(..),
  (?.=),
  stripNullFields
) where

import Data.Aeson
import Data.Aeson.Types
import RIO

import qualified Data.Aeson.KeyMap as KM

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
-- Prefer @stripNullFields@ to this. 
(?.=) :: (Foldable t, ToJSON (t v)) => Key -> t v -> [Pair]
key ?.= value
  | null value = []
  | otherwise = [key .= value]

class Pairs a where
  pairs :: a -> [Pair]

-- | Strips null fields from JSON recursively. 
stripNullFields :: Value -> Value
stripNullFields (Object o) = Object $ KM.foldMapWithKey stripNull o 
  where
    stripNull k v
      | Null <- v = mempty
      | otherwise = KM.singleton k (stripNullFields v) 
stripNullFields j = j