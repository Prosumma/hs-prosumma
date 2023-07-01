{-# LANGUAGE NamedFieldPuns #-}

module Prosumma.Logging (
  initDefaultLogging,
  newLogger,
  withInitLogging,
  withLogging,
  Logger(..)
) where

import RIO

newtype Logger = Logger { loggerLogFunc :: LogFunc }

newLogger :: Logger
newLogger = Logger mempty

instance HasLogFunc Logger where
  logFuncL = lens loggerLogFunc (\context loggerLogFunc -> context{loggerLogFunc})

initDefaultLogging :: IO LogOptions
initDefaultLogging = logOptionsHandle stderr True <&> setLogUseTime True . setLogUseLoc True 

withInitLogging :: HasLogFunc s => IO LogOptions -> RIO s a -> RIO s a
withInitLogging initLogging app = liftIO initLogging >>= withLogging app

withLogging :: HasLogFunc s => RIO s a -> LogOptions -> RIO s a
withLogging app options = do
  state <- ask
  liftIO $ withLogFunc options $ \logFunc -> do
    let loggingState = state & logFuncL .~ logFunc
    runRIO loggingState app