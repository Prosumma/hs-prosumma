{-# LANGUAGE DataKinds, FlexibleContexts, RankNTypes #-}

module Prosumma.Servant (
  defaultExceptionHandler,
  loggingExceptionHandler,
  mapApp,
  mapServerException,
  runApp,
  runApplication,
  ServerExceptionHandler,
  ServerHandler,
  ServerResponse
) where

import RIO hiding (Handler)
import Servant

type ServerResponse a = Either ServerError a
type ServerHandler a = Handler (ServerResponse a)
type ServerExceptionHandler s a = SomeException -> RIO s (ServerResponse a)

mapServerException :: SomeException -> ServerResponse a 
mapServerException e = case fromException e :: Maybe ServerError of 
  Just se -> Left se
  Nothing -> Left err500

defaultExceptionHandler :: SomeException -> RIO s (ServerResponse a)
defaultExceptionHandler = return . mapServerException

loggingExceptionHandler :: (HasLogFunc s) => SomeException -> RIO s (ServerResponse a)
loggingExceptionHandler e = do
  logError $ displayShow e
  return $ mapServerException e

runApp :: ServerExceptionHandler s a -> s -> RIO s a -> ServerHandler a
runApp handler state app = liftIO $ runRIO state $ catch (Right <$> app) handler

mapApp :: ServerExceptionHandler s a -> s -> RIO s a -> Handler a
mapApp handler state app = runApp handler state app >>= mapIntoHandler
  where
    mapIntoHandler r = case r of
      Left e -> throwError e
      Right a -> return a

runApplication :: HasServer api '[] => (forall a. ServerExceptionHandler s a) -> Proxy api -> ServerT api (RIO s) -> s -> Application
runApplication handler proxy api state = serve proxy $ hoistServer proxy (mapApp handler state) api