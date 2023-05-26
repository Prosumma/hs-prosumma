{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, TypeFamilies #-}

module Prosumma.AWS (
  Env,
  HasAWSEnv(..),
  sendAWS,
  sendAWSThrowOnError
) where

import Amazonka
import Control.Monad.Reader
import Data.Generics.Product.Fields
import Prosumma.Exceptions
import RIO

class HasAWSEnv a where
  getAWSEnv :: a -> Env

instance HasAWSEnv Env where
  getAWSEnv = id

sendAWS :: (
    MonadUnliftIO m,
    AWSRequest r,
    Typeable r,
    Typeable (AWSResponse r),
    MonadReader env m,
    HasAWSEnv env
  ) => r -> m (AWSResponse r)
sendAWS r = do
  env <- asks getAWSEnv
  runResourceT $ send env r

sendAWSThrowOnError :: (
  MonadUnliftIO m,
  AWSRequest rq,
  Typeable rq,
  MonadThrow m,
  MonadReader env m,
  HasAWSEnv env,
  HasField "httpStatus" rs rs Int Int,
  Typeable rs,
  Exception e, rs ~ AWSResponse rq
  ) => (Int -> e) -> rq -> m rs 
sendAWSThrowOnError mkException = throwOnHttpStatusError mkException <=< sendAWS