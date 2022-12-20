module Prosumma.Exceptions (
  MatchException,
  catchMatch,
  matchException,
  throwMatch,
  whenException
) where

import Prosumma.Logging
import RIO hiding (log)
import Text.Printf

type MatchException source destination = source -> Either destination source

matchException :: (Exception source, Exception destination) => MatchException source destination -> MatchException SomeException SomeException 
matchException handler source = case fromException source of
  Nothing -> return source
  Just e -> bimap toException toException $ handler e 

whenException :: (source -> Bool) -> destination -> MatchException source destination 
whenException cond destination source = if cond source then Left destination else Right source

throwMatch :: (MonadThrow m, HasLogging m, MonadIO m) => MatchException SomeException SomeException -> SomeException -> m a
throwMatch match e = do
  log $ printf "throwMatch: %s" (show e)
  case match e of
    Left e -> throwM e
    Right e -> throwM e

catchMatch :: (MonadUnliftIO m, MonadThrow m, HasLogging m) => MatchException SomeException SomeException -> m a -> m a
catchMatch match action = catchAny action $ throwMatch match 
