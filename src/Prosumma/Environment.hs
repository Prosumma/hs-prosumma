{-|
Module: Prosumma.Environment

This module copies relevant functions from Boris Buliga's excellent
env-extra library (https://hackage.haskell.org/package/env-extra).

Unfortunately env-extra is incompatible with the extra library
(https://hackage.haskell.org/package/extra), which I also use,
so I had to copy the relevant functions here.
-}
module Prosumma.Environment (envRead, envMaybe, envString, envValue, MissingEnvError(..)) where

import Control.Monad.IO.Class
import Data.Maybe
import Data.String
import Data.Text
import Data.Text.Read
import Prosumma.Util
import RIO hiding (Reader)
import System.Environment

newtype MissingEnvError = MissingEnvError Text deriving (Show)
instance Exception MissingEnvError

-- | Gets an environment variable as a string-like value.
--
--
-- The first argument is a default value. If the environment variable is not found,
-- then the default is used. If the provided default is `Nothing`, then a
-- `MissingEnvError` is thrown with the given key.
envString :: (MonadIO m, IsString a) => Maybe a -> Text -> m a
envString def key = whenNothingM (envMaybe key <&> (?? def)) $
  throwIO $ MissingEnvError key

-- | Gets an environment variable.
--
--
-- The first argument is a default value. If the environment variable is not found,
-- then the default is used. If the provided default is `Nothing`, then a
-- `MissingEnvError` is thrown with the given key.
envValue :: MonadIO m => Maybe a -> Reader a -> Text -> m a
envValue def r key = whenNothingM (envRead r key <&> (?? def)) $
  throwIO $ MissingEnvError key

envMaybe :: (MonadIO m, IsString a) => Text -> m (Maybe a)
envMaybe key = liftIO $ fromString <<$>> lookupEnv (unpack key)

envRead :: (MonadIO m) => Reader a -> Text -> m (Maybe a)
envRead r = fmap (((fmap fst . hush) . r) =<<) . envMaybe