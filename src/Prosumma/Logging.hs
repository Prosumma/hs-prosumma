module Prosumma.Logging (
  initDefaultLogging,
  withDefaultLogging,
  withInitLogging,
  withLogFunc,
  withLogging
) where

import RIO hiding (withLogFunc)

import qualified RIO

initDefaultLogging :: IO LogOptions
initDefaultLogging = logOptionsHandle stderr True

withInitLogging :: HasLogFunc s => IO LogOptions -> RIO s a -> RIO s a
withInitLogging initLogging app = liftIO initLogging >>= withLogging app

-- | Creates and sets the @LogFunc@ in @s@, then calls @runRIO@.
withLogging :: HasLogFunc s => RIO s a -> LogOptions -> RIO s a
withLogging app options = do
  state <- ask
  liftIO $ RIO.withLogFunc options $ \logFunc -> do
    let loggingState = state & logFuncL .~ logFunc
    runRIO loggingState app

withDefaultLogging :: HasLogFunc s => RIO s a -> RIO s a
withDefaultLogging = withInitLogging initDefaultLogging

-- | This function is used to forward the @LogFunc@, usually to another Monad
--
-- For an example, see @Prosumma.PG.run@.
withLogFunc :: (MonadReader env m, HasLogFunc env) => (LogFunc -> m a) -> m a
withLogFunc action = asks (^.logFuncL) >>= action 