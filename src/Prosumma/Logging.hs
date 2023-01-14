module Prosumma.Logging (
  HasLogging(..),
  log
) where

import RIO hiding (log)

class HasLogging m where
  allowLogging :: m Bool

-- TODO: Use RIO's logging infastructure.
log :: (MonadIO m, HasLogging m) => IO () -> m ()
log io = whenM allowLogging $ liftIO io
