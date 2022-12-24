{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Prosumma.AWS (HasEnv(..), MonadAWS(..), AWS, runAWS, sendAWSRequest) where

import Amazonka
import Control.Monad.IO.Class
import Control.Monad.Reader
import RIO

class HasEnv m where
  getEnv :: m Env

class Monad m => MonadAWS m where
  sendAWS :: (AWSRequest r) => r -> m (AWSResponse r)

newtype AWS a = AWS { unAWS :: ReaderT Env IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

runAWS :: MonadIO m => Env -> AWS a -> m a
runAWS env = liftIO . flip runReaderT env . unAWS

sendAWSRequest :: (MonadUnliftIO m, HasEnv m, AWSRequest r) => r -> m (AWSResponse r)
sendAWSRequest r = do
  env <- getEnv
  runResourceT $ send env r

instance HasEnv AWS where
  getEnv = AWS ask

instance MonadUnliftIO AWS where
  withRunInIO action = AWS $ withRunInIO $ \run -> action (run . unAWS)

instance MonadAWS AWS where
  sendAWS = sendAWSRequest 