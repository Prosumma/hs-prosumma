module Prosumma.Logging (
  initDefaultLogging,
  withInitLogging,
  withLogging
) where

import RIO

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