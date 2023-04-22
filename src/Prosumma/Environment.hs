{-|
Module: Prosumma.Environment

This module copies relevant functions from Boris Buliga's excellent
env-extra library (https://hackage.haskell.org/package/env-extra).

Unfortunately env-extra is incompatible with the extra library
(https://hackage.haskell.org/package/extra), which I also use,
so I had to copy the relevant functions here.
-}
module Prosumma.Environment (envRead, envMaybe, envJust) where

import Control.Monad.IO.Class
import Data.Maybe
import Data.String
import Data.Text 
import Data.Text.Read
import Prosumma.Util
import RIO hiding (fromRight, Reader)
import System.Environment

envJust :: (MonadIO m, IsString a) => Text -> m a
envJust = fromJust <<$>> envMaybe

envMaybe :: (MonadIO m, IsString a) => Text -> m (Maybe a)
envMaybe key = liftIO $ fromString <<$>> lookupEnv (unpack key)

envRead :: (MonadIO m) => Reader a -> Text -> m (Maybe a)
envRead r = fmap (((fmap fst . fromRight) . r) =<<) . envMaybe

fromRight :: Either a b -> Maybe b
fromRight = either (const Nothing) Just
