module Prosumma.Auth.Client (
  addContact,
  checkHealth,
  createUser,
  validateCode,
  validatePassword
) where

import Data.Aeson
import Prosumma.Auth.API
import Prosumma.Auth.Types
import Servant
import Servant.Client

validateCode :: ValidateCodeRequest -> ClientM ValidateCodeResponse
validatePassword :: ValidatePasswordRequest -> ClientM Value
createUser :: CreateUserRequest -> ClientM CreateUserResponse
addContact :: AddContactRequest -> ClientM AddContactResponse
checkHealth :: ClientM NoContent
validateCode :<|> validatePassword :<|> createUser :<|> addContact :<|> checkHealth = client apiProxy