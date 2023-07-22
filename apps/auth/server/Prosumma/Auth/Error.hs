{-# LANGUAGE FlexibleContexts #-}

module Prosumma.Auth.Error (
  catchDefault,
  matchDefaultExceptions
) where

import Database.PostgreSQL.Simple (SqlError(..))
import Prosumma
import RIO 
import Servant
import Text.Regex.TDFA

matchByState :: ByteString -> HelpMatchException SqlError ServerError
matchByState code = throwWhen $ \sqe -> sqlState sqe == code

matchByRegex :: ByteString -> Text -> HelpMatchException SqlError ServerError
matchByRegex code regex = throwWhen $ \sqe -> (sqlState sqe == code) && (sqlErrorMsg sqe =~ regex)

matchDefaultSqlExceptions :: MatchException SomeException SomeException
matchDefaultSqlExceptions = matchException $
  matchByRegex "23514" "contact_contact_check" err422 { errReasonPhrase = "The supplied contact has an invalid format." } >=>
  matchByState "23505" err409 >=>
  matchByState "23514" err422 >=>
  matchByState "BANND" err401 { errReasonPhrase = "The user is banned." } >=>
  matchByState "NOAPP" err422 { errReasonPhrase = "The specified app does not exist." } >=>
  matchByState "NOCOD" err401 { errReasonPhrase = "The given code does not exist or has expired." } >=>
  matchByState "NOPAS" err401 { errReasonPhrase = "No password found." } >=>
  matchByState "NOUSR" err422 { errReasonPhrase = "The specified user does not exist." } >=>
  matchByState "PNDNG" err403 { errReasonPhrase = "The user is pending." }

matchDefaultExceptions :: MatchException SomeException SomeException
matchDefaultExceptions = matchDefaultSqlExceptions

catchDefault :: (MonadUnliftIO m, MonadThrow m) => m a -> m a
catchDefault = catchMatch matchDefaultExceptions