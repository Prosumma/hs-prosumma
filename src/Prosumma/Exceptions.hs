{-# LANGUAGE DataKinds, FlexibleContexts, TypeApplications #-}

module Prosumma.Exceptions (
  HelpMatchException,
  MatchException,
  catchLog,
  catchMatch,
  liftEitherM,
  matchAll,
  matchException,
  throwOnHttpStatusError,
  throwOnHttpStatusOutsideRange,
  throwMatch,
  throwWhen
) where

import Control.Monad.Error.Class
import Data.Generics.Product.Fields
import RIO 

type MatchException original matched = original -> Either matched original
type HelpMatchException original matched = matched -> MatchException original matched

-- | Attempts to match `SomeException` to the type of `original`.
--
-- If a match occurs, then `handler` is executed. The `handler` can further examine the exception to
-- decide whether it is a match. If it is, it should throw that exception, i.e., return it as `Left`
-- to short-circuit all matching. Otherwise, it should return the original as `Right` so that it can be passed
-- to the next handler. Handlers are typically stitched together using `>=>`:
--
-- > matchSqlState :: ByteString -> ServerError -> MatchException SqlError ServerError
-- > matchSqlState code = throwWhen $ \sqe -> sqlState sqe == code 
-- >
-- > matchNoApp :: MatchException SqlError ServerError 
-- > matchNoApp = matchSqlState "NOAPP" err404 { errReasonPhrase = "The specified app does not exist." } 
-- >
-- > matchNoUsr :: MatchException SqlError ServerError
-- > matchNoUsr = matchSqlState "NOUSR" err404 { errReasonPhrase = "The specified user does not exist." }  
-- >
-- > matchException (matchNoApp >=> matchNoUsr)
matchException :: (Exception original, Exception matched) => MatchException original matched -> MatchException SomeException SomeException 
matchException handler original = case fromException original of
  Nothing -> return original
  Just e -> bimap toException toException $ handler e 

-- | A combinator useful for building `MatchException`s.
-- 
-- > matchSqlState :: ByteString -> ServerError -> MatchException SqlError ServerError
-- > matchSqlState code = throwWhen $ \sqe -> sqlState sqe == code
throwWhen :: (original -> Bool) -> HelpMatchException original matched 
throwWhen cond matched original = if cond original then throwError matched else return original

-- | Used by `catchMatch` to attempt to match an exception. 
--
-- `throwMatch` passes the original exception through the match chain, rethrowing the left-hand side if a match occurred
-- or the right-hand side if no match occurred. A match chain typically consists of a series of `MatchException` instances
-- joined by `>=>`.
throwMatch :: MonadThrow m => MatchException SomeException SomeException -> SomeException -> m a
throwMatch match e = do
  case match e of
    Left e -> throwM e
    Right e -> throwM e

-- | Attempts to perform `action`. If an exception is thrown, it uses `throwMatch` to attempt to match and rethrow
-- the exception if needed.
--
-- This is typically used to match underlying errors -- often `SqlError` instances thrown by database operations -- onto `ServerError` instances required by Servant.
--
-- > defaultSqlToServerErrors :: MatchException SomeException SomeException
-- > defaultSqlToServerErrors = matchException $ matchNoApp >=> matchNoUsr
-- >
-- > catchMatch defaultSqlToServerErrors getUser
catchMatch :: (MonadUnliftIO m, MonadThrow m) => MatchException SomeException SomeException -> m a -> m a
catchMatch match action = catchAny action $ throwMatch match 

-- | Allows the specification of a fallback exception.
--
-- > matchAll (ServerError 500) >=> defaultExceptions
matchAll :: Exception e => e -> MatchException SomeException SomeException
matchAll = const . throwError . toException 

-- | Throws an exception if the HTTP Status falls outside the given range, otherwise returns its last argument.
throwOnHttpStatusOutsideRange :: (MonadThrow m, Exception e, HasField "httpStatus" r r Int Int) => [Int] -> (Int -> e) -> r -> m r 
throwOnHttpStatusOutsideRange range mkException response = let httpStatus = response^.(field @"httpStatus") in
  if httpStatus `notElem` range
    then throwM $ mkException httpStatus
    else return response

-- | Throws an exception if the HTTP status falls outside the range 200..299.
throwOnHttpStatusError :: (MonadThrow m, Exception e, HasField "httpStatus" r r Int Int) => (Int -> e) -> r -> m r 
throwOnHttpStatusError = throwOnHttpStatusOutsideRange [200..299]

-- | Maps from `Either` to `MonadThrow`.
liftEitherM :: (MonadThrow m, Exception e) => Either e a -> m a
liftEitherM either = case either of
  Left e -> throwM e
  Right a -> return a

catchLog :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => m a -> m a
catchLog action = catch action $ \(e :: SomeException) -> logError (displayShow e) >> throwIO e 