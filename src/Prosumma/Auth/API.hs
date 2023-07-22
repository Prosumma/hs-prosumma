{-# LANGUAGE DataKinds, FlexibleContexts, MultiParamTypeClasses, TypeOperators #-}

module Prosumma.Auth.API (
  apiProxy,
  AddContact,
  CreateUser,
  ValidateCode,
  ValidatePassword,
  API
) where

import Data.Aeson
import Prosumma.Auth.Types
import RIO
import Servant

type CreateUser = "user" :> ReqBody '[JSON] CreateUserRequest :> Post '[JSON] CreateUserResponse
type AddContact = "contact" :> ReqBody '[JSON] AddContactRequest :> Post '[JSON] AddContactResponse
type ValidateCode = "code" :> ReqBody '[JSON] ValidateCodeRequest :> Post '[JSON] ValidateCodeResponse
type ValidatePassword = "password" :> ReqBody '[JSON] ValidatePasswordRequest :> Post '[JSON] Value
type HealthCheck = "health" :> Get '[PlainText] NoContent 

type API = "v1" :> (ValidateCode :<|> ValidatePassword :<|> CreateUser :<|> AddContact :<|> HealthCheck)

apiProxy :: Proxy API
apiProxy = Proxy
