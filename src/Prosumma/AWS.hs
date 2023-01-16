{-# LANGUAGE FlexibleInstances #-}

module Prosumma.AWS (
  HasAWSEnv(..),
  sendAWS
) where

import Amazonka
import Control.Monad.Reader
import RIO

class HasAWSEnv a where
  getAWSEnv :: a -> Env

instance HasAWSEnv Env where
  getAWSEnv = id

sendAWS :: (MonadUnliftIO m, AWSRequest r, MonadReader env m, HasAWSEnv env) => r -> m (AWSResponse r)
sendAWS r = do
  env <- asks getAWSEnv
  runResourceT $ send env r
