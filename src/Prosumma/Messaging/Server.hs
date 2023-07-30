module Prosumma.Messaging.Server (
  apiProxy,
  runApplication,
  runServer
) where

import Data.Text.Read
import Formatting
import Network.Wai.Handler.Warp
import Prosumma
import Prosumma.Logging
import Prosumma.Messaging.API
import Prosumma.Messaging.App
import Prosumma.Servant
import RIO hiding (Handler)
import Servant

healthCheck :: Monad m => m NoContent 
healthCheck = return NoContent

api :: ServerT API App
api = healthCheck 

apiProxy :: Proxy API
apiProxy = Proxy

runServer :: IO ()
runServer = runRIO newLogger $ withDefaultLogging $ do
  logInfo $ displayText "Starting server."
  port <- envValue (Just 8080) decimal "PROSUMMA_PORT"
  logInfo $ uformat ("Using port " % int % ".") port
  logFunc <- asks (^.logFuncL)
  ctx <- newAppContext
  liftIO $ run port $ runApplicationWithLogFunc logFunc apiProxy api ctx