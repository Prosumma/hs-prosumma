{-# LANGUAGE DataKinds, FlexibleContexts, KindSignatures, RankNTypes, TypeOperators #-}

module Prosumma.Servant (
  defaultExceptionHandler,
  loggingExceptionHandler,
  mapServerException,
  maybeThrow404,
  runApplication,
  runApplicationWithContext,
  ServerExceptionHandler,
  ServerHandler,
  ServerResponse,
  StateTransform,
) where

import Control.Monad.Error.Class
import Data.Kind
import Prosumma.Util
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

defaultExceptionHandler :: ServerExceptionHandler s a 
defaultExceptionHandler = return . mapServerException

loggingExceptionHandler :: HasLogFunc s => ServerExceptionHandler s a
loggingExceptionHandler e = do
  logError $ displayShow e
  defaultExceptionHandler e

runApp :: ServerExceptionHandler s a -> StateTransform s a -> s -> RIO s a -> ServerHandler a
runApp handler transform state app = liftIO $ runRIO state $ catch (Right <$> transform app) handler

mapApp :: ServerExceptionHandler s a -> StateTransform s a -> s -> RIO s a -> Handler a
mapApp handler transform state app = runApp handler transform state app >>= liftEither 

runApplication
  :: forall (api :: Type) s. HasServer api '[]
  => (forall a. ServerExceptionHandler s a) -> (forall a. StateTransform s a) -> Proxy api -> ServerT api (RIO s) -> s -> Application
runApplication handler transform proxy api state = serve proxy $ hoistServer proxy (mapApp handler transform state) api

runApplicationWithContext
  :: forall (api :: Type) (context :: [Type]) s. (HasServer api context, HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters)
  => Context context -> (forall a. ServerExceptionHandler s a) -> (forall a. StateTransform s a) -> Proxy api -> ServerT api (RIO s) -> s -> Application
runApplicationWithContext context handler transform proxy api state =
  serveWithContext proxy context $ hoistServerWithContext proxy (Proxy :: Proxy context) (mapApp handler transform state) api

maybeThrow404 :: MonadIO m => m (Maybe a) -> m a
maybeThrow404 action = action >>= flip whenNothing (throwIO err404)