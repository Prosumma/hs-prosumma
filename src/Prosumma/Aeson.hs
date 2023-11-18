module Prosumma.Aeson (
  Pairs(..),
  (?.=),
) where

import Data.Aeson
import Data.Aeson.Types
import RIO

infixr 8 ?.= 

(?.=) :: (Foldable t, ToJSON (t v)) => Key -> t v -> [Pair]
key ?.= value
  | null value = []
  | otherwise = [key .= value]

class Pairs a where
  pairs :: a -> [Pair]