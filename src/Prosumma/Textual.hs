module Prosumma.Textual (
  Textual(..)
) where

import RIO

class Textual a where
  fromText :: Text -> Maybe a