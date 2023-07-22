module Prosumma.Auth.Database (
  addContact,
  createUser,
  getPassword,
  validateCode
) where

import Data.UUID
import Database.PostgreSQL.Simple (Binary(..))
import Prosumma.Auth.Types
import Prosumma.PG
import RIO

createUser :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m, MonadThrow m) => EncryptedUser -> m Text 
createUser = value1 "SELECT create_user(?, ?, ?)"

addContact :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m, MonadThrow m) => AddContactRequest -> m Text 
addContact = value1 "SELECT add_contact(?, ?, ?)"

getPassword :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m, MonadThrow m) => ValidatePasswordRequest -> m (Binary ByteString) 
getPassword = value1 "SELECT get_password(?, ?)"

validateCode :: (MonadReader env m, ConnectionRunner env, HasLogFunc env, MonadIO m, MonadThrow m) => ValidateCodeRequest -> m UUID 
validateCode = value1 "SELECT validate_code(?, ?, ?)"
