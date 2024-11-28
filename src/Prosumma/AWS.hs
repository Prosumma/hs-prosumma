{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, TypeFamilies, TypeOperators #-}

module Prosumma.AWS (
  sendAWS,
  sendAWSThrowOnError,
  sendAWSThrowOnStatus,
  AWS(..),
  Env,
  HasAWSEnv(..)
) where

import Amazonka
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Type.Equality
import Data.Generics.Product.Fields
import Prosumma.Exceptions
import RIO

class HasAWSEnv a where
  getAWSEnv :: a -> Env

instance HasAWSEnv Env where
  getAWSEnv = id

data AWS = AWS {
  awsEnv :: !Env,
  awsLogFunc :: !LogFunc
}

instance HasAWSEnv AWS where
  getAWSEnv = awsEnv

instance HasLogFunc AWS where
  logFuncL = lens awsLogFunc $ \aws awsLogFunc -> aws{awsLogFunc}

sendAWS
  :: (MonadResource m, AWSRequest r, Typeable r, Typeable (AWSResponse r), MonadReader env m, HasAWSEnv env)
  => r -> m (AWSResponse r)
sendAWS r = do
  env <- asks getAWSEnv
  send env r

sendAWSThrowOnError
  :: (MonadResource m, AWSRequest rq, Typeable rq, MonadThrow m, MonadReader env m, HasAWSEnv env, HasField "httpStatus" rs rs Int Int, Typeable rs, Exception e, rs ~ AWSResponse rq)
  => (Int -> e) -> rq -> m rs 
sendAWSThrowOnError mkException = throwOnHttpStatusError mkException <=< sendAWS

sendAWSThrowOnStatus
  :: (MonadResource m, AWSRequest rq, Typeable rq, MonadThrow m, MonadReader env m, HasAWSEnv env, HasField "httpStatus" rs rs Int Int, Typeable rs, rs ~ AWSResponse rq)
  => rq -> m rs 
sendAWSThrowOnStatus = sendAWSThrowOnError HTTPStatusException