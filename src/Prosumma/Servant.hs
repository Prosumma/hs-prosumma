{-# LANGUAGE DataKinds, FlexibleContexts, KindSignatures, RankNTypes #-}

module Prosumma.Servant (
  defaultExceptionHandler,
  initDefaultLogging,
  loggingExceptionHandler,
  mapServerException,
  runApplication,
  runApplicationWithDefaultLogging,
  runApplicationWithLogging,
  runApplicationWithoutLogging,
  withInitLogging,
  ServerExceptionHandler,
  ServerHandler,
  ServerResponse,
  StateTransform,
) where

import Control.Monad.Error.Class
import Data.Kind
import RIO hiding (Handler)
import Servant

type ServerResponse a = Either ServerError a
type ServerHandler a = Handler (ServerResponse a)
type ServerExceptionHandler s a = SomeException -> RIO s (ServerResponse a)
type StateTransform s a = RIO s a -> RIO s a

mapServerException :: SomeException -> ServerResponse a 
mapServerException e = case fromException e :: Maybe ServerError of 
  Just se -> Left se
  Nothing -> Left err500

defaultExceptionHandler :: SomeException -> RIO s (ServerResponse a)
defaultExceptionHandler = return . mapServerException

loggingExceptionHandler :: HasLogFunc s => SomeException -> RIO s (ServerResponse a)
loggingExceptionHandler e = do
  logError $ displayShow e
  return $ mapServerException e

initDefaultLogging :: IO LogOptions
initDefaultLogging = logOptionsHandle stderr True <&> setLogUseTime True . setLogUseLoc True 

withLogging :: HasLogFunc s => RIO s a -> LogOptions -> RIO s a
withLogging app options = do
  state <- ask
  liftIO $ withLogFunc options $ \logFunc -> do
    let loggingState = state & logFuncL .~ logFunc
    runRIO loggingState app

withInitLogging :: HasLogFunc s => IO LogOptions -> RIO s a -> RIO s a
withInitLogging initLogging app = liftIO initLogging >>= withLogging app

runApp :: ServerExceptionHandler s a -> StateTransform s a -> s -> RIO s a -> ServerHandler a
runApp handler transform state app = liftIO $ runRIO state $ catch (Right <$> transform app) handler

mapApp :: ServerExceptionHandler s a -> StateTransform s a -> s -> RIO s a -> Handler a
mapApp handler transform state app = runApp handler transform state app >>= liftEither 

runApplication ::
  forall (api :: Type) s.
  HasServer api '[] =>
  (forall a. ServerExceptionHandler s a) -> (forall a. StateTransform s a) -> Proxy api -> ServerT api (RIO s) -> s -> Application
runApplication handler transform proxy api state = serve proxy $ hoistServer proxy (mapApp handler transform state) api

runApplicationWithLogging ::
  forall (api :: Type) s.
  (HasLogFunc s, HasServer api '[]) => 
  IO LogOptions -> Proxy api -> ServerT api (RIO s) -> s -> Application 
runApplicationWithLogging initLogging = runApplication loggingExceptionHandler (withInitLogging initLogging)

runApplicationWithDefaultLogging :: forall (api :: Type) s. (HasLogFunc s, HasServer api '[]) => Proxy api -> ServerT api (RIO s) -> s -> Application
runApplicationWithDefaultLogging = runApplicationWithLogging initDefaultLogging 

runApplicationWithoutLogging :: forall (api :: Type) s. HasServer api '[] => Proxy api -> ServerT api (RIO s) -> s -> Application
runApplicationWithoutLogging = runApplication defaultExceptionHandler id