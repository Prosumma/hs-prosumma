{-# LANGUAGE DataKinds, FlexibleContexts, MultiParamTypeClasses, RecordWildCards #-}

module Prosumma.Auth.Server (
  api,
  apiProxy,
  runServer
) where

import Data.Aeson
import Data.String.Conversions
import Data.Text.Read
import Database.PostgreSQL.Simple (Binary(..))
import Formatting
import Network.Wai.Handler.Warp
import Prosumma
import Prosumma.Auth.App
import Prosumma.Auth.API
import Prosumma.Auth.Error
import Prosumma.Auth.Types
import Prosumma.Crypto
import Prosumma.Logging
import Prosumma.Servant
import RIO hiding (Handler, log)
import Servant

import qualified Prosumma.Auth.Database as DB

createUser :: CreateUserRequest -> App CreateUserResponse
createUser user = catchDefault $ encryptUser user >>= writeToDatabase <&> CreateUserResponse
  where
    encryptUser CreateUserRequest{..} = do
      encryptedPassword <- catchLog (mapM (encryptMessage . convertString) curqPassword)
      return $ EncryptedUser curqApp (Binary <$> encryptedPassword) curqContact
    writeToDatabase encryptedUser = catchLog (DB.createUser encryptedUser)

addContact :: AddContactRequest -> App AddContactResponse
addContact contact = catchDefault $ DB.addContact contact <&> AddContactResponse

validateCode :: ValidateCodeRequest -> App ValidateCodeResponse
validateCode request = catchDefault $ DB.validateCode request <&> ValidateCodeResponse

validatePassword :: ValidatePasswordRequest -> App Value
validatePassword request = do
  let receivedPassword = convertString $ request^.password
  Binary encryptedPassword <- catchDefault $ DB.getPassword request
  decryptedPassword <- decryptMessage encryptedPassword
  if receivedPassword == decryptedPassword
    then return $ object []
    else throwM err401

checkHealth :: App NoContent
checkHealth = return NoContent

api :: ServerT API App
api = validateCode :<|> validatePassword :<|> createUser :<|> addContact :<|> checkHealth

runServer :: IO ()
runServer = runRIO newLogger $ withDefaultLogging $ do
  logInfo $ displayText "Starting server."
  port <- envValue (Just 8080) decimal "PROSUMMA_PORT"
  logInfo $ uformat ("Using port " % int % ".") port
  logFunc <- asks (^.logFuncL)
  context <- fromEnvironment
  logInfo $ displayText "Ready for connections."
  liftIO $ run port $ runApplicationWithLogFunc logFunc apiProxy api context