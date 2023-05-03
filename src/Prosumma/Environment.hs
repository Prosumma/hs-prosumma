module Prosumma.Environment (environment, HasEnvironment(..)) where

import RIO
import RIO.Text

class HasEnvironment a where
  vpc :: a -> Text
  namespace :: a -> Text

environment :: HasEnvironment a => a -> Text
environment a = intercalate "." [vpc a, namespace a]