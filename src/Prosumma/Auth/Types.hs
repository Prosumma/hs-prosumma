{-# LANGUAGE DeriveGeneric, FlexibleInstances, FunctionalDependencies, RecordWildCards, TemplateHaskell #-}

module Prosumma.Auth.Types (
  AddContactRequest(..),
  AddContactResponse(..),
  CreateUserRequest(..),
  CreateUserResponse(..),
  EncryptedUser(..),
  ValidateCodeRequest(..),
  ValidateCodeResponse(..),
  ValidatePasswordRequest(..),
  app,
  code,
  contact,
  identifier,
  password
) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Data.Aeson
import Data.UUID
import GHC.Generics
import Prosumma
import RIO

data CreateUserRequest = CreateUserRequest {
  curqApp :: !Text,
  curqPassword :: !(Maybe Text),
  curqContact :: !Text
} deriving (Show, Generic)

makeProsummaLenses ''CreateUserRequest

instance FromJSON CreateUserRequest where
  parseJSON = withObject "CreateUserRequest" $ \o -> CreateUserRequest
    <$> o .: "app"
    <*> o .:? "password"
    <*> o .: "contact"

instance ToJSON CreateUserRequest where
  toJSON CreateUserRequest{..} = object [
      "app" .= curqApp,
      "password" .= curqPassword,
      "contact" .= curqContact
    ]

data EncryptedUser = EncryptedUser {
  euApp :: !Text,
  euPassword :: !(Maybe (Binary ByteString)),
  euContact :: !Text
} deriving (Show, Generic)

makeProsummaLenses ''EncryptedUser

instance ToRow EncryptedUser where
  toRow EncryptedUser{..} = [
      toField euApp,
      toField euPassword,
      toField euContact
    ]

newtype CreateUserResponse = CreateUserResponse Text deriving (Show, Generic)

instance ToJSON CreateUserResponse where
  toJSON (CreateUserResponse code) = object [
      "code" .= code
    ]

instance FromJSON CreateUserResponse where
  parseJSON = withObject "CreateUserResponse" $ \u -> CreateUserResponse <$> u .: "code"

data AddContactRequest = AddContactRequest {
  acrqApp :: !Text,
  acrqIdentifier :: !UUID,
  acrqContact :: !Text
} deriving (Show, Generic)

makeProsummaLenses ''AddContactRequest

instance FromJSON AddContactRequest where
  parseJSON = withObject "AddContactRequest" $ \o -> AddContactRequest
    <$> o .: "app"
    <*> o .: "identifier"
    <*> o .: "contact"

instance ToJSON AddContactRequest where
  toJSON AddContactRequest{..} = object [
      "app" .= acrqApp,
      "identifier" .= acrqIdentifier,
      "contact" .= acrqContact
    ]

instance ToRow AddContactRequest where
  toRow AddContactRequest{..} = [
      toField acrqApp,
      toField acrqIdentifier,
      toField acrqContact
    ]

newtype AddContactResponse = AddContactResponse Text deriving (Show, Generic)

instance FromJSON AddContactResponse where
  parseJSON = withObject "AddContactResponse" $ \c -> AddContactResponse <$> c .: "code"

instance ToJSON AddContactResponse where
  toJSON (AddContactResponse code) = object [ "code" .= code ]

data ValidateCodeRequest = ValidateCodeRequest {
  vcrqApp :: !Text,
  vcrqContact :: !Text,
  vcrqCode :: !Text
} deriving (Generic, Show)

makeProsummaLenses ''ValidateCodeRequest

instance FromJSON ValidateCodeRequest where
  parseJSON = withObject "ValidateCodeRequest" $ \r -> ValidateCodeRequest
    <$> r .: "app"
    <*> r .: "contact"
    <*> r .: "code"

instance ToJSON ValidateCodeRequest where
  toJSON ValidateCodeRequest{..} = object [
      "app" .= vcrqApp,
      "contact" .= vcrqContact,
      "code" .= vcrqCode
    ]

instance ToRow ValidateCodeRequest where
  toRow ValidateCodeRequest{..} = [
      toField vcrqApp,
      toField vcrqContact,
      toField vcrqCode
    ]

newtype ValidateCodeResponse = ValidateCodeResponse UUID deriving (Show, Generic)

instance FromJSON ValidateCodeResponse where
  parseJSON = withObject "ValidateCodeResponse" $ \v -> ValidateCodeResponse <$> v .: "identifier"

instance ToJSON ValidateCodeResponse where
  toJSON (ValidateCodeResponse identifier) = object [ "identifier" .= identifier ]

data ValidatePasswordRequest = ValidatePasswordRequest {
  vprqApp :: !Text,
  vprqContact :: !Text,
  vprqPassword :: !Text
} deriving (Generic, Show)

makeProsummaLenses ''ValidatePasswordRequest

instance FromJSON ValidatePasswordRequest where
  parseJSON = withObject "ValidatePasswordRequest" $ \p -> ValidatePasswordRequest
    <$> p .: "app"
    <*> p .: "contact"
    <*> p .: "password"

instance ToJSON ValidatePasswordRequest where
  toJSON ValidatePasswordRequest{..} = object [
      "app" .= vprqApp,
      "contact" .= vprqContact,
      "password" .= vprqPassword
    ]

instance ToRow ValidatePasswordRequest where
  toRow ValidatePasswordRequest{..} = [
      toField vprqApp,
      toField vprqContact
      -- We deliberately exclude password here
    ]
